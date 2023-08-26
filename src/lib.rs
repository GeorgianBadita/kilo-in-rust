use std::{fs, io};
use std::cmp::{max, min};
use std::fs::File;
use std::io::{BufRead, BufReader, Read, stdin, stdout, Write};
use std::os::fd::{AsFd, RawFd};
use std::os::unix::fs::OpenOptionsExt;
use std::path::Path;
use std::string::ToString;
use std::time::SystemTime;

use scanf::sscanf;
use termios::*;

const KILO_VERSION: &str = "0.0.1";
const KILO_TAB_STOP: usize = 8;
const KILO_QUIT_TIMES: usize = 3;

const HL_HIGHLIGHT_NUMBERS: i32 = 1 << 0;
const HL_HIGHLIGHT_STRINGS: i32 = 1 << 1;

const CTRL_Q: char = (b'q' & 0x1f) as char;
const CTRL_H: char = (b'h' & 0x1f) as char;
const CTRL_L: char = (b'l' & 0x1f) as char;
const CTRL_S: char = (b's' & 0x1f) as char;
const CTRL_F: char = (b'f' & 0x1f) as char;

pub fn erase_screen_for_exit() {
    stdout().write_all("\x1b[2J".as_bytes()).unwrap();
    stdout().write_all("\x1b[H".as_bytes()).unwrap();
    stdout().flush().unwrap();
}

fn is_separator(c: char) -> bool {
    c.is_ascii_whitespace() || c == '\0' || ",.()+-/*=~%<>[];".contains(c)
}

fn editor_find_callback(editor: &mut Editor, query: &String, key: EditorKey) {
    if !editor.search_state.saved_hl.is_empty() {
        editor.rows[editor.search_state.saved_hl_line as usize].hl = editor.search_state.saved_hl.clone();
        editor.search_state.saved_hl.clear();
    }

    if key == EditorKey::Key('\x1b') || key == EditorKey::Key('\r') {
        editor.search_state.direction = 1;
        editor.search_state.last_match = -1;
        return;
    } else if key == EditorKey::ArrowRight
        || key == EditorKey::ArrowDown {
        editor.search_state.direction = 1;
    } else if key == EditorKey::ArrowLeft
        || key == EditorKey::ArrowUp {
        editor.search_state.direction = -1;
    } else {
        editor.search_state.direction = 1;
        editor.search_state.last_match = -1;
    }

    if editor.search_state.last_match == -1 {
        editor.search_state.direction = 1;
    }

    let mut current = editor.search_state.last_match;

    for _ in 0..editor.num_rows() {
        current += editor.search_state.direction;
        if current == -1 {
            current = editor.num_rows() as i32 - 1;
        } else if current == editor.num_rows() as i32 {
            current = 0;
        }

        let num_rows = editor.num_rows();
        let row = &mut editor.rows[current as usize];

        if let Some(pos) = row.render.find(query) {
            editor.search_state.last_match = current;
            editor.cy = current as usize;
            editor.cx = row.editor_row_rx_to_cx(pos);
            editor.row_off = num_rows;

            editor.search_state.saved_hl_line = current;
            editor.search_state.saved_hl = row.hl.clone();
            for idx in pos..pos + query.len() {
                row.hl[idx] = EditorHighlight::Match;
            }
            break;
        }
    }
}

#[derive(Clone)]
pub struct TermSettings(Termios);

impl TermSettings {
    pub fn from_fd(fd: RawFd) -> io::Result<TermSettings> {
        let term = TermSettings(Termios::from_fd(fd)?);
        term.enable_raw_mode()?;
        Ok(term)
    }

    fn enable_raw_mode(&self) -> io::Result<()> {
        let mut raw_term = self.0;
        // Input flags
        raw_term.c_iflag &= !(IXON | ICRNL | INPCK | ISTRIP | BRKINT);

        // Output flags
        raw_term.c_oflag &= !OPOST;

        // Local flags
        raw_term.c_lflag &= !(ECHO | ICANON | ISIG | IEXTEN);

        // Control flags
        raw_term.c_cflag |= CS8;

        // Set control chars
        raw_term.c_cc[VMIN] = 0;
        raw_term.c_cc[VTIME] = 1;

        // Applying new terminal settings
        tcsetattr(libc::STDIN_FILENO, TCSANOW, &raw_term)
    }
}

impl Drop for TermSettings {
    fn drop(&mut self) {
        erase_screen_for_exit();
        tcsetattr(libc::STDIN_FILENO, TCSANOW, &self.0).unwrap();
    }
}


#[derive(Debug, PartialEq)]
enum EditorKey {
    ArrowLeft,
    ArrowRight,
    ArrowUp,
    ArrowDown,
    Backspace,
    DelKey,
    HomeKey,
    EndKey,
    PageUp,
    PageDown,
    Key(char),
}

struct Row {
    row: String,
    render: String,
    hl: Vec<EditorHighlight>,
}

impl Row {
    fn from_string(row: &str, editor_syntax: &Option<&Syntax>) -> Row {
        let (render, highlight) = Self::update_row(row, editor_syntax);
        Row {
            row: row.to_string(),
            render,
            hl: highlight,
        }
    }

    pub fn insert_at_pos(&mut self, pos: usize, ch: u8, editor_syntax: &Option<&Syntax>) {
        String::insert(&mut self.row, pos, ch as char);
        let updated_row = &self.row;
        let (render, highlight) = Self::update_row(updated_row, editor_syntax);
        self.render = render;
        self.hl = highlight;
    }

    pub fn delete_at_pos(&mut self, pos: usize, editor_syntax: &Option<&Syntax>) {
        String::remove(&mut self.row, pos);
        let updated_row = &self.row;
        let (render, highlight) = Self::update_row(updated_row, editor_syntax);
        self.render = render;
        self.hl = highlight;
    }

    fn update_row(row_str: &str, editor_syntax: &Option<&Syntax>) -> (String, Vec<EditorHighlight>) {
        let mut render = String::new();
        for char in row_str.chars() {
            if char == '\t' {
                render.push(' ');
                while render.len() % KILO_TAB_STOP != 0 { render.push(' '); }
            } else {
                render.push(char);
            }
        }
        let highlight = Self::update_syntax(&render, editor_syntax);
        (render, highlight)
    }

    fn update_syntax(render: &str, editor_syntax: &Option<&Syntax>) -> Vec<EditorHighlight> {
        let mut highlight = vec![EditorHighlight::Normal; render.len()];

        if editor_syntax.as_ref().is_none() {
            return highlight;
        }

        let syntax = editor_syntax.as_ref().unwrap();
        let keywords1 = syntax.keywords1.clone();
        let keywords2 = syntax.keywords2.clone();

        let scs = syntax.single_comment_start.clone();

        let mut prev_step = true;
        let mut in_string = 0;

        let mut idx = 0;
        let chars: Vec<char> = render.chars().collect();
        while idx < chars.len() {
            let ch = chars[idx];
            let prev_hl = if idx > 0 { highlight[idx - 1].clone() } else { EditorHighlight::Normal };

            if !scs.is_empty() && in_string == 0 && render[idx..].starts_with(scs) {
                for hl in highlight.iter_mut().take(render.len()).skip(idx) {
                    *hl = EditorHighlight::Comment;
                }
                break;
            }

            if editor_syntax.as_ref().unwrap().flags & HL_HIGHLIGHT_STRINGS != 0 {
                if in_string != 0 {
                    highlight[idx] = EditorHighlight::String;
                    if ch == '\\' && idx + 1 < chars.len() {
                        highlight[idx + 1] = EditorHighlight::String;
                        idx += 1;
                        continue;
                    }
                    if ch as u8 == in_string {
                        in_string = 0;
                    }
                    idx += 1;
                    prev_step = true;
                    continue;
                } else if ch == '"' || ch == '\'' {
                    in_string = ch as u8;
                    highlight[idx] = EditorHighlight::String;
                    idx += 1;
                    continue;
                }
            }

            if editor_syntax.as_ref().unwrap().flags & HL_HIGHLIGHT_NUMBERS != 0 &&
                ((ch.is_ascii_digit() && (prev_step || prev_hl == EditorHighlight::Number)) ||
                    (ch == '.' && prev_hl == EditorHighlight::Number)) {
                highlight[idx] = EditorHighlight::Number;
                idx += 1;
                prev_step = false;
                continue;
            }

            if prev_step {
                for kw1 in keywords1.iter() {
                    if render[idx..].starts_with(kw1) {
                        for hl in highlight.iter_mut().skip(idx).take(kw1.len()) {
                            *hl = EditorHighlight::Keyword1;
                        }
                        idx += kw1.len();
                        break;
                    }
                }

                for kw2 in keywords2.iter() {
                    if render[idx..].starts_with(kw2) {
                        for hl in highlight.iter_mut().skip(idx).take(kw2.len()) {
                            *hl = EditorHighlight::Keyword2;
                        }

                        idx += kw2.len();
                        break;
                    }
                }
            }

            prev_step = is_separator(ch);
            idx += 1
        }
        highlight
    }

    fn editor_row_cx_to_rx(&self, cx: usize) -> usize {
        let mut rx = 0;
        let row: Vec<char> = self.row.chars().collect();
        for ch in row.iter().take(cx) {
            if *ch == '\t' {
                rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
            }
            rx += 1;
        }
        rx
    }

    fn editor_row_rx_to_cx(&self, rx: usize) -> usize {
        let mut cur_rx = 0;
        let row: Vec<char> = self.row.chars().collect();

        for (cx, r) in row.iter().enumerate().take(self.row.len()) {
            if *r == '\t' {
                cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
            }
            cur_rx += 1;
            if cur_rx > rx {
                return cx;
            }
        }
        self.row.len() - 1
    }
}

struct SearchState {
    last_match: i32,
    direction: i32,
    saved_hl_line: i32,
    saved_hl: Vec<EditorHighlight>,
}

impl Default for SearchState {
    fn default() -> Self {
        Self {
            last_match: -1,
            direction: 1,
            saved_hl_line: 0,
            saved_hl: Vec::new(),
        }
    }
}

#[derive(Clone)]
struct Syntax<'a> {
    file_type: &'a str,
    file_match: &'a [&'a str],
    single_comment_start: &'a str,
    keywords1: &'a [&'a str],
    keywords2: &'a [&'a str],
    flags: i32,
}

const HLDB: [&Syntax; 1] = [&Syntax {
    file_type: "rs",
    file_match: &["rs"],
    single_comment_start: "//",
    keywords1: &["abstract", "alignof", "as", "become", "box", "break", "const", "continue",
        "crate", "do", "else", "enum", "extern", "false", "final", "fn", "for", "if",
        "impl", "in", "let", "loop", "macro", "match", "mod", "move", "mut", "offsetof",
        "override", "priv", "proc", "pub", "pure", "ref", "return", "Self", "self",
        "sizeof", "static", "struct", "super", "trait", "true", "type", "typeof",
        "unsafe", "unsized", "use", "virtual", "where", "while", "yield", "bool", "char",
        "str"],
    keywords2: &["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "isize", "usize", "f32",
        "f64", "str"],
    flags: HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
}];

#[derive(Clone, PartialEq)]
enum EditorHighlight {
    Normal,
    Comment,
    Keyword1,
    Keyword2,
    String,
    Number,
    Match,
}

impl EditorHighlight {
    fn to_color(&self) -> usize {
        match self {
            EditorHighlight::Comment => 36,
            EditorHighlight::Keyword1 => 33,
            EditorHighlight::Keyword2 => 32,
            EditorHighlight::String => 35,
            EditorHighlight::Number => 31,
            EditorHighlight::Match => 34,
            _ => 37
        }
    }
}

pub struct Editor {
    _term_settings: TermSettings,
    screen_rows: usize,
    screen_cols: usize,
    cx: usize,
    cy: usize,
    rx: usize,
    rows: Vec<Row>,
    row_off: usize,
    col_off: usize,
    filename: Option<String>,
    status_msg: String,
    message_time: u64,
    dirty: bool,
    quit_tries: usize,
    search_state: SearchState,
    curr_editor_syntax: Option<&'static Syntax<'static>>,
}

impl Editor {
    pub fn from_settings(term_settings: TermSettings) -> Result<Self, &'static str> {
        let win_size_res = get_window_size()?;
        let (screen_cols, screen_rows) = win_size_res;

        Ok(Editor {
            _term_settings: term_settings,
            screen_cols,
            screen_rows: screen_rows - 2,
            cx: 0,
            cy: 0,
            rx: 0,
            rows: Vec::new(),
            row_off: 0,
            col_off: 0,
            filename: None,
            status_msg: String::new(),
            message_time: 0,
            dirty: false,
            quit_tries: KILO_QUIT_TIMES,
            search_state: SearchState::default(),
            curr_editor_syntax: None,
        })
    }

    pub fn editor_open(&mut self, file_path: &str) {
        self.filename = Some(file_path.to_string());
        self.select_syntax_highlight();
        let file = fs::File::open(file_path).unwrap_or_else(|e| panic!("{}", e));
        let buff = BufReader::new(file);
        buff.lines()
            .filter(|r| r.is_ok())
            .map(|r| r.unwrap().trim_end().to_string())
            .for_each(|line| self.insert_row(self.num_rows(), &line));
        self.dirty = false;
    }

    pub fn refresh_screen(&mut self) {
        self.editor_scroll();
        let mut buff: Vec<u8> = Vec::new();
        buff.extend("\x1b[?25l".as_bytes());
        buff.extend("\x1b[H".as_bytes());

        self.editor_draw_rows(&mut buff);
        self.editor_draw_status_bar(&mut buff);
        self.editor_draw_message_bar(&mut buff);

        buff.extend(format!("\x1b[{};{}H",
                            (self.cy as i32 - self.row_off as i32) + 1,
                            (self.rx as i32 - self.col_off as i32) + 1).as_bytes());
        buff.extend("\x1b[?25h".as_bytes());

        stdout().write_all(&buff).unwrap();
        stdout().flush().unwrap();
    }

    pub fn editor_process_key_press(&mut self) -> bool {
        let mut io_in = stdin();
        let c = Self::editor_read_key(&mut io_in);
        match c {
            EditorKey::Backspace => {
                self.del_char();
            }
            EditorKey::DelKey => {
                self.editor_move_cursor(EditorKey::ArrowRight);
                self.del_char();
            }
            EditorKey::HomeKey => {
                self.cx = 0;
            }
            EditorKey::EndKey => {
                if self.cx < self.num_rows() {
                    self.cx = self.rows[self.cy].row.len();
                }
            }
            EditorKey::PageUp | EditorKey::PageDown => {
                if c == EditorKey::PageUp {
                    self.cy = self.row_off;
                } else if c == EditorKey::PageDown {
                    self.cy = self.row_off + self.screen_rows - 1;
                    if self.cy > self.num_rows() {
                        self.cy = self.num_rows();
                    }
                }

                let mut times = self.screen_rows;
                while times > 0 {
                    times -= 1;
                    self.editor_move_cursor(if c == EditorKey::PageUp
                    { EditorKey::ArrowUp } else { EditorKey::ArrowDown })
                }
            }
            EditorKey::ArrowLeft | EditorKey::ArrowRight | EditorKey::ArrowUp | EditorKey::ArrowDown => self.editor_move_cursor(c),
            EditorKey::Key('\r') => {
                self.insert_new_line()
            }
            EditorKey::Key('\x1b') => {}
            EditorKey::Key(c) if c == CTRL_S => {
                self.editor_save()
                    .unwrap_or_else(|err| self.editor_set_status_message(&format!("Error saving file: {}", err)));
            }
            EditorKey::Key(c) if c == CTRL_F => {
                self.editor_find();
            }
            EditorKey::Key(c) if c == CTRL_H => {
                self.del_char();
            }
            EditorKey::Key(c) if c == CTRL_L => {
                // nothing
            }
            EditorKey::Key(c) if c == CTRL_Q => {
                if self.dirty && self.quit_tries > 0 {
                    self.editor_set_status_message(&format!("WARNING!!! File has unsaved changes. Press Ctrl-Q {} more times to quit.", self.quit_tries));
                    self.quit_tries -= 1;
                    return false;
                }
                erase_screen_for_exit();
                return true;
            }
            EditorKey::Key(c) => {
                self.insert_char(c as u8);
            }
        }
        self.quit_tries = KILO_QUIT_TIMES;
        false
    }

    fn rows_to_string(&self) -> String {
        self.rows.iter().map(|r| &*r.row).collect::<Vec<&str>>().join("\n")
    }

    fn select_syntax_highlight(&mut self) {
        self.curr_editor_syntax = None;
        if let Some(filename) = self.filename.as_ref() {
            let ext = Path::new(filename).extension();
            if let Some(extension) = ext {
                let ext_str = extension.to_str().unwrap();
                for hl_entry in HLDB {
                    if hl_entry.file_match.contains(&ext_str) {
                        self.curr_editor_syntax = Some(hl_entry);
                        let new_rows: Vec<Row> = self.rows.iter().map(|r| {
                            Row::from_string(&r.row, &self.curr_editor_syntax)
                        }).collect();
                        self.rows = new_rows;
                        break;
                    }
                }
            }
        }
    }

    fn editor_find(&mut self) {
        let cx = self.cx;
        let cy = self.cy;
        let row_off = self.row_off;
        let col_off = self.col_off;

        let query = self.editor_prompt("Search (Use ESC/Arrows/Enter)", Some(editor_find_callback));
        // reset cursor state
        if query.is_none() {
            self.cx = cx;
            self.cy = cy;
            self.row_off = row_off;
            self.col_off = col_off;
        }
    }

    fn editor_save(&mut self) -> io::Result<()> {
        if self.filename.is_none() {
            let user_filename = self.editor_prompt("Save as (ESC to cancel)", None::<fn(&mut _, &_, _)>);
            if let Some(fname) = user_filename {
                self.filename = Some(fname);
            } else {
                self.editor_set_status_message("Save aborted");
                return Ok(());
            }
            self.select_syntax_highlight();
        }

        let filename = self.filename.as_ref().unwrap().clone();
        let rows_to_string = self.rows_to_string();
        let mut file = File::options()
            .read(true)
            .write(true)
            .create(true)
            .mode(0o644)
            .open(filename)?;
        let num_bytes = rows_to_string.as_bytes().len();
        file.set_len(num_bytes as u64)?;
        file.write_all(rows_to_string.as_bytes())?;
        self.editor_set_status_message(&format!("{} bytes saved to disk", num_bytes));
        self.dirty = false;

        Ok(())
    }

    pub fn editor_set_status_message(&mut self, message: &str) {
        self.status_msg = message.to_string();
        self.message_time = get_sys_time_in_secs();
    }

    fn insert_char(&mut self, ch: u8) {
        if self.cy == self.num_rows() {
            self.insert_row(self.num_rows(), "");
        }
        self.rows[self.cy].insert_at_pos(self.cx, ch, &self.curr_editor_syntax);
        self.cx += 1;
        self.dirty = true;
    }

    fn del_char(&mut self) {
        if self.cy == self.num_rows() {
            return;
        }
        if self.cy == 0 && self.cx == 0 {
            return;
        }

        if self.cx > 0 {
            self.rows[self.cy].delete_at_pos(self.cx - 1, &self.curr_editor_syntax);
            self.cx -= 1;
            self.dirty = true;
        } else {
            self.cx = self.rows[self.cy - 1].row.len();
            let last_row = self.rows[self.cy - 1].row.as_str();
            let curr_row = self.rows[self.cy].row.as_str();
            let new_row = Row::from_string(&(last_row.to_string() + curr_row), &self.curr_editor_syntax);
            self.rows[self.cy - 1] = new_row;
            self.del_row(self.cy);
            self.cy -= 1;
            self.dirty = true;
        }
    }

    fn insert_row(&mut self, row_idx: usize, row_content: &str) {
        if row_idx > self.num_rows() {
            return;
        }
        Vec::insert(&mut self.rows, row_idx, Row::from_string(row_content, &self.curr_editor_syntax));
        self.dirty = true;
    }

    fn del_row(&mut self, row_idx: usize) {
        if row_idx >= self.num_rows() { return; }
        Vec::remove(&mut self.rows, row_idx);
        self.dirty = true;
    }

    fn insert_new_line(&mut self) {
        if self.cx == 0 {
            self.insert_row(self.cy, "");
        } else {
            let row_string = &self.rows[self.cy].row.clone();
            self.insert_row(self.cy + 1, &row_string[self.cx..]);
            let row = self.rows[self.cy].row[..self.cx].to_string();
            self.rows[self.cy] = Row::from_string(row.as_str(), &self.curr_editor_syntax);
        }
        self.cy += 1;
        self.cx = 0;
    }

    fn editor_prompt<G>(&mut self, prompt: &str, callback: Option<G>) -> Option<String>
        where G: Fn(&mut Editor, &String, EditorKey)
    {
        let mut buff = String::new();
        loop {
            self.editor_set_status_message(&format!("{}: {}", prompt, buff));
            self.refresh_screen();

            let c = Self::editor_read_key(&mut stdin());
            if EditorKey::Key('\x1b') == c {
                self.editor_set_status_message("");
                if callback.is_some() {
                    callback.as_ref().unwrap()(self, &buff, EditorKey::Key('\x1b'));
                }
                return None;
            } else if EditorKey::Key('\r') == c && !buff.is_empty() {
                if callback.is_some() {
                    callback.as_ref().unwrap()(self, &buff, EditorKey::Key('r'));
                }
                return Some(buff);
            }
            match c {
                EditorKey::DelKey | EditorKey::Backspace => {
                    if !buff.is_empty() {
                        buff.pop();
                    }
                }
                EditorKey::Key(key) if key == CTRL_H => {
                    if !buff.is_empty() {
                        buff.pop();
                    }
                }
                EditorKey::Key(key)  if !key.is_ascii_control() && (key as u8) < 128 => {
                    buff.push(key);
                }
                _ => {}
            };

            if callback.is_some() {
                callback.as_ref().unwrap()(self, &buff, c);
            }
        }
    }

    fn editor_read_key(io_in: &mut impl Read) -> EditorKey {
        fn read_one(io_in: &mut impl Read) -> Option<u8> {
            let mut buff: [u8; 1] = [0];
            match io_in.take(1).read(&mut buff) {
                Ok(1) => Some(buff[0]),
                Ok(0) => None,
                Ok(_) | Err(_) => panic!("Error on read")
            }
        }

        let c;
        loop {
            match read_one(io_in) {
                Some(char) => {
                    c = char;
                    break;
                }
                None => continue
            }
        }

        if c == b'\x1b' {
            let first_opt = read_one(io_in);
            let second_opt = read_one(io_in);

            if first_opt.is_none() || second_opt.is_none() {
                return EditorKey::Key('\x1b');
            }
            let first = first_opt.unwrap();
            let second = second_opt.unwrap();

            if first as char == '[' {
                if second as char >= '0' && second as char <= '9' {
                    let third = read_one(io_in).unwrap();

                    if third as char == '~' {
                        if second as char == '1' {
                            return EditorKey::HomeKey;
                        } else if second as char == '3' {
                            return EditorKey::DelKey;
                        } else if second as char == '4' {
                            return EditorKey::EndKey;
                        } else if second as char == '5' {
                            return EditorKey::PageUp;
                        } else if second as char == '6' {
                            return EditorKey::PageDown;
                        } else if second as char == '7' {
                            return EditorKey::HomeKey;
                        } else if second as char == '8' {
                            return EditorKey::EndKey;
                        }
                    }
                } else {
                    match second as char {
                        'A' => return EditorKey::ArrowUp,
                        'B' => return EditorKey::ArrowDown,
                        'C' => return EditorKey::ArrowRight,
                        'D' => return EditorKey::ArrowLeft,
                        'H' => return EditorKey::HomeKey,
                        'F' => return EditorKey::EndKey,
                        _ => {}
                    }
                }
            } else if first as char == 'O' {
                if second as char == 'H' {
                    return EditorKey::HomeKey;
                } else if second as char == 'F' {
                    return EditorKey::EndKey;
                }
            }

            return EditorKey::Key('\x1b');
        }

        // Backspace
        if c == 127 {
            return EditorKey::Backspace;
        }
        EditorKey::Key(c as char)
    }

    fn editor_move_cursor(&mut self, key: EditorKey) {
        let mut row = None;
        if self.cy < self.num_rows() {
            row = Some(&self.rows[self.cy]);
        }

        match key {
            EditorKey::ArrowLeft => {
                if self.cx != 0 {
                    self.cx -= 1;
                } else if self.cy > 0 {
                    self.cy -= 1;
                    if self.cy < self.rows.len() {
                        self.cx = self.rows[self.cy].row.len();
                    }
                }
            }
            EditorKey::ArrowRight => {
                if row.is_some() && self.cx < row.unwrap().row.len() {
                    self.cx += 1;
                } else {
                    self.cy += 1;
                    self.cx = 0;
                }
            }
            EditorKey::ArrowUp if self.cy != 0 => self.cy -= 1,
            EditorKey::ArrowDown  if self.cy < self.num_rows() => self.cy += 1,
            _ => {}
        }

        let len = if self.cy >= self.num_rows()
        { 0 } else { self.rows[self.cy].row.len() };
        if self.cx > len {
            self.cx = len;
        }
    }

    fn editor_draw_message_bar(&mut self, buff: &mut Vec<u8>) {
        buff.extend("\x1b[K".as_bytes());
        let msg_len = min(self.status_msg.len(), self.screen_cols);
        if msg_len > 0 && get_sys_time_in_secs() - self.message_time < 5 {
            buff.extend(self.status_msg.as_bytes());
        }
    }

    fn editor_scroll(&mut self) {
        self.rx = 0;
        if self.cy < self.num_rows() {
            self.rx = self.rows[self.cy].editor_row_cx_to_rx(self.cx);
        }

        if self.cy < self.row_off {
            self.row_off = self.cy;
        }

        if self.cy >= self.row_off + self.screen_rows {
            self.row_off += self.cy - self.screen_rows + 1;
        }

        if self.cx < self.col_off {
            self.col_off = self.cx;
        }

        if self.cx >= self.col_off + self.screen_cols {
            self.col_off = self.cx - self.screen_cols + 1;
        }

        if self.rx < self.col_off {
            self.col_off = self.rx;
        }

        if self.rx >= self.col_off + self.screen_cols {
            self.col_off = self.rx - self.screen_cols + 1;
        }
    }

    fn editor_draw_rows(&self, buff: &mut Vec<u8>) {
        for y in 0..self.screen_rows {
            let filerow = y + self.row_off;
            if filerow >= self.num_rows() {
                if self.num_rows() == 0 && y == self.screen_rows / 3 {
                    let mut welcome_message = String::from("Kilo editor -- version ") + KILO_VERSION;
                    if welcome_message.len() > self.screen_cols {
                        welcome_message = welcome_message[..self.screen_cols].to_string();
                    }
                    let mut padding = (self.screen_cols as i32 - welcome_message.len() as i32) / 2;
                    if padding > 0 {
                        buff.push(b'~');
                        padding -= 1;
                    }
                    buff.extend(" ".repeat(padding as usize).as_bytes());
                    buff.extend(welcome_message.as_bytes());
                } else {
                    buff.push(b'~');
                }
            } else {
                let len = min(
                    max(self.rows[filerow].render.len() as i32 - self.col_off as i32, 0),
                    self.screen_cols as i32) as usize;
                let row_num = filerow;
                let col_num = self.col_off;
                if len != 0 {
                    let hl = &self.rows[row_num].hl[self.col_off..];
                    let mut current_color = 0;
                    for (idx, char) in self.rows[row_num].render[col_num..col_num + len].to_string().chars().enumerate() {
                        if char.is_control() {
                            let sym = if char as u8 <= 26 { b'@' + char as u8 } else { b'?' };
                            buff.extend("\x1b[7m".as_bytes());
                            buff.push(sym);
                            buff.extend("\x1b[m".as_bytes());
                            if current_color != 0 {
                                buff.extend(format!("\x1b[%{}m", current_color).as_bytes());
                            }
                        } else if hl[idx] == EditorHighlight::Normal {
                            if current_color != 0 {
                                buff.extend("\x1b[39m".as_bytes());
                                current_color = 0;
                            }
                            buff.push(char as u8);
                        } else {
                            let color = hl[idx].to_color();
                            if color != current_color {
                                current_color = color;
                                buff.extend(format!("\x1b[{}m", color).as_bytes());
                            }
                            buff.push(char as u8);
                        }
                    }
                    buff.extend("\x1b[39m".as_bytes());
                }
            }
            buff.extend("\x1b[K".as_bytes());
            buff.extend("\r\n".as_bytes());
        }
    }

    fn editor_draw_status_bar(&self, buff: &mut Vec<u8>) {
        buff.extend("\x1b[7m".as_bytes());
        let num_rows = self.rows.len();
        let status_bytes = format!("{:.30} - {} lines {}",
                                   if let Some(filename) = self.filename.as_ref()
                                   { filename } else { "[No Name]" }, num_rows,
                                   if self.dirty { "(modified)" } else { "" }).to_string();

        let mut len = min(status_bytes.len(), self.screen_cols);

        let line_info_bytes = format!("{} | {}/{}",
                                      if let Some(syntax) = self.curr_editor_syntax.as_ref()
                                      { syntax.file_type } else { "no file type" },
                                      self.cy + 1, self.num_rows()).to_string();
        let info_len = line_info_bytes.len();

        buff.extend(status_bytes[..len].as_bytes());
        while len < self.screen_cols {
            if self.screen_cols - len == info_len {
                buff.extend(line_info_bytes.as_bytes());
                break;
            } else {
                buff.extend(" ".as_bytes());
                len += 1;
            }
        }

        buff.extend("\x1b[m".as_bytes());
        buff.extend("\r\n".as_bytes());
    }

    fn num_rows(&self) -> usize {
        self.rows.len()
    }
}

fn get_cursor_position() -> Result<(usize, usize), &'static str> {
    stdout().write_all("\x1b[6n".as_bytes()).unwrap();
    stdout().flush().unwrap();

    let mut buff: [u8; 32] = [0; 32];
    let chars_read = stdin().read(&mut buff).unwrap();

    if buff[0] as char != '\x1b' || buff[1] as char != '[' {
        return Err("Wrong buffer for cursor pointer position");
    }
    let mut rows: i32 = 1;
    let mut cols: i32 = 1;
    let string_res = buff[2..chars_read - 1].iter().map(|x| *x as char).collect::<String>();
    let scanf_ret = sscanf!(&string_res, "{};{}", rows, cols);

    if scanf_ret.is_err() {
        return Err("Error parsing cursor position from scanf");
    }
    Ok((cols as usize, rows as usize))
}

fn get_window_size() -> Result<(usize, usize), &'static str> {
    let ioctl_res = rustix::termios::tcgetwinsize(stdout().as_fd());
    if let Ok(r) = ioctl_res {
        return Ok((r.ws_col as usize, r.ws_row as usize));
    }
    // Go down and right 999 cols
    stdout().write_all("\x1b[999C\x1b[999B".as_bytes()).unwrap_or_else(|e| {
        erase_screen_for_exit();
        panic!("{}", e)
    });
    stdout().flush().unwrap_or_else(|e| {
        erase_screen_for_exit();
        panic!("{}", e);
    });
    return get_cursor_position();
}

fn get_sys_time_in_secs() -> u64 {
    match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => n.as_secs(),
        Err(_) => panic!("SystemTime before UNIX EPOCH!"),
    }
}