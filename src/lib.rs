use std::{fs, io};
use std::cmp::{max, min};
use std::fs::File;
use std::io::{BufRead, BufReader, Read, stdin, stdout, Write};
use std::os::fd::{AsFd, RawFd};
use std::os::unix::fs::OpenOptionsExt;
use std::time::SystemTime;

use scanf::sscanf;
use termios::*;

const KILO_VERSION: &str = "0.0.1";
const KILO_TAB_STOP: usize = 8;
const KILO_QUIT_TIMES: usize = 3;

const CTRL_Q: char = (b'q' & 0x1f) as char;
const CTRL_H: char = (b'h' & 0x1f) as char;
const CTRL_L: char = (b'l' & 0x1f) as char;
const CTRL_S: char = (b's' & 0x1f) as char;

pub fn erase_screen_for_exit() {
    stdout().write_all("\x1b[2J".as_bytes()).unwrap();
    stdout().write_all("\x1b[H".as_bytes()).unwrap();
    stdout().flush().unwrap();
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
enum EditorSpecialKey {
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
}

#[derive(Debug, PartialEq)]
enum EditorKey {
    SpecialKey(EditorSpecialKey),
    Key(char),
}

struct Row {
    row: String,
    render: String,
}

impl Row {
    fn from_string(row: &str) -> Row {
        Row {
            row: row.to_string(),
            render: Self::update_row(row),
        }
    }

    pub fn insert_at_pos(&mut self, pos: usize, ch: u8) {
        String::insert(&mut self.row, pos, ch as char);
        let updated_row = &self.row;
        self.render = Self::update_row(updated_row);
    }

    pub fn delete_at_pos(&mut self, pos: usize) {
        String::remove(&mut self.row, pos);
        let updated_row = &self.row;
        self.render = Self::update_row(updated_row);
    }

    fn update_row(row_str: &str) -> String {
        let mut render = String::new();
        for char in row_str.chars() {
            if char == '\t' {
                render.push(' ');
                while render.len() % KILO_TAB_STOP != 0 { render.push(' '); }
            } else {
                render.push(char);
            }
        }
        render
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
    dirty: usize,
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
            dirty: 0,
        })
    }


    pub fn editor_open(&mut self, file_path: &str) {
        self.filename = Some(file_path.to_string());
        let file = fs::File::open(file_path).unwrap_or_else(|e| panic!("{}", e));
        let buff = BufReader::new(file);
        buff.lines()
            .filter(|r| r.is_ok())
            .map(|r| r.unwrap().trim_end().to_string())
            .for_each(|line| self.insert_row(self.num_rows(), &line));
        self.dirty = 0;
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
        static mut QUIT_TIMES: usize = KILO_QUIT_TIMES;

        let mut io_in = stdin();
        let c = Self::editor_read_key(&mut io_in);
        match c {
            EditorKey::SpecialKey(EditorSpecialKey::Backspace) => {
                self.del_char();
            }
            EditorKey::SpecialKey(EditorSpecialKey::DelKey) => {
                self.editor_move_cursor(EditorSpecialKey::ArrowRight);
                self.del_char();
            }
            EditorKey::SpecialKey(EditorSpecialKey::HomeKey) => {
                self.cx = 0;
            }
            EditorKey::SpecialKey(EditorSpecialKey::EndKey) => {
                if self.cx < self.num_rows() {
                    self.cx = self.rows[self.cy].row.len();
                }
            }
            EditorKey::SpecialKey(EditorSpecialKey::PageUp) | EditorKey::SpecialKey(EditorSpecialKey::PageDown) => {
                if c == EditorKey::SpecialKey(EditorSpecialKey::PageUp) {
                    self.cy = self.row_off;
                } else if c == EditorKey::SpecialKey(EditorSpecialKey::PageDown) {
                    self.cy = self.row_off + self.screen_rows - 1;
                    if self.cy > self.num_rows() {
                        self.cy = self.num_rows();
                    }
                }

                let mut times = self.screen_rows;
                while times > 0 {
                    times -= 1;
                    self.editor_move_cursor(if c == EditorKey::SpecialKey(EditorSpecialKey::PageUp)
                    { EditorSpecialKey::ArrowUp } else { EditorSpecialKey::ArrowDown })
                }
            }
            EditorKey::SpecialKey(key) => self.editor_move_cursor(key),
            EditorKey::Key('\r') => {
                self.insert_new_line()
            }
            EditorKey::Key('\x1b') => {}
            EditorKey::Key(c) if c == CTRL_S => {
                self.editor_save()
                    .unwrap_or_else(|err| self.editor_set_status_message(&format!("Error saving file: {}", err)));
            }
            EditorKey::Key(c) if c == CTRL_H => {
                self.del_char();
            }
            EditorKey::Key(c) if c == CTRL_L => {
                // nothing
            }
            EditorKey::Key(c) if c == CTRL_Q => unsafe {
                if self.dirty > 0 && QUIT_TIMES > 0 {
                    self.editor_set_status_message(&format!("WARNING!!! File has unsaved changes. Press Ctrl-Q {} more times to quit.", QUIT_TIMES));
                    QUIT_TIMES -= 1;
                    return false;
                }
                erase_screen_for_exit();
                return true;
            }
            EditorKey::Key(c) => {
                self.insert_char(c as u8);
            }
        }
        // TODO: find a way to remove the unsafe blocks
        // Right now it's fine as there will be only one thread modifying this
        unsafe { QUIT_TIMES = KILO_QUIT_TIMES; }
        false
    }

    fn rows_to_string(&self) -> String {
        self.rows.iter().map(|r| &*r.row).collect::<Vec<&str>>().join("\n")
    }

    fn editor_save(&mut self) -> io::Result<()> {
        let filename;
        if self.filename.is_none() {
            let user_filename = self.editor_prompt("Save as (ESC to cancel)");
            if let Some(fname) = user_filename {
                filename = fname;
            } else {
                self.editor_set_status_message("Save aborted");
                return Ok(());
            }
        } else {
            filename = self.filename.clone().unwrap();
        }
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
        self.dirty = 0;

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
        self.rows[self.cy].insert_at_pos(self.cx, ch);
        self.cx += 1;
        self.dirty += 1;
    }

    fn del_char(&mut self) {
        if self.cy == self.num_rows() {
            return;
        }
        if self.cy == 0 && self.cx == 0 {
            return;
        }

        if self.cx > 0 {
            self.rows[self.cy].delete_at_pos(self.cx - 1);
            self.cx -= 1;
            self.dirty += 1;
        } else {
            self.cx = self.rows[self.cy - 1].row.len();
            let last_row = self.rows[self.cy - 1].row.as_str();
            let curr_row = self.rows[self.cy].row.as_str();
            self.rows[self.cy - 1] = Row::from_string(&(last_row.to_string() + curr_row));
            self.del_row(self.cy);
            self.cy -= 1;
            self.dirty += 1;
        }
    }

    fn insert_row(&mut self, row_idx: usize, row_content: &str) {
        if row_idx > self.num_rows() {
            return;
        }

        Vec::insert(&mut self.rows, row_idx, Row::from_string(row_content));
        self.dirty += 1;
    }

    fn del_row(&mut self, row_idx: usize) {
        if row_idx >= self.num_rows() { return; }
        Vec::remove(&mut self.rows, row_idx);
        self.dirty += 1;
    }

    fn insert_new_line(&mut self) {
        if self.cx == 0 {
            self.insert_row(self.cy, "");
        } else {
            let row_string = &self.rows[self.cy].row.clone();
            self.insert_row(self.cy + 1, &row_string[self.cx..]);
            let row = self.rows[self.cy].row[..self.cx].to_string();
            self.rows[self.cy] = Row::from_string(row.as_str());
        }
        self.cy += 1;
        self.cx = 0;
    }

    fn editor_prompt(&mut self, prompt: &str) -> Option<String> {
        let mut buff = String::new();
        loop {
            self.editor_set_status_message(&format!("{}: {}", prompt, buff));
            self.refresh_screen();

            let c = Self::editor_read_key(&mut stdin());
            if EditorKey::Key('\x1b') == c {
                self.editor_set_status_message("");
                return None;
            } else if EditorKey::Key('\r') == c && !buff.is_empty() {
                return Some(buff);
            }
            match c {
                EditorKey::SpecialKey(EditorSpecialKey::DelKey) | EditorKey::SpecialKey(EditorSpecialKey::Backspace) => {
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
                            return EditorKey::SpecialKey(EditorSpecialKey::HomeKey);
                        } else if second as char == '3' {
                            return EditorKey::SpecialKey(EditorSpecialKey::DelKey);
                        } else if second as char == '4' {
                            return EditorKey::SpecialKey(EditorSpecialKey::EndKey);
                        } else if second as char == '5' {
                            return EditorKey::SpecialKey(EditorSpecialKey::PageUp);
                        } else if second as char == '6' {
                            return EditorKey::SpecialKey(EditorSpecialKey::PageDown);
                        } else if second as char == '7' {
                            return EditorKey::SpecialKey(EditorSpecialKey::HomeKey);
                        } else if second as char == '8' {
                            return EditorKey::SpecialKey(EditorSpecialKey::EndKey);
                        }
                    }
                } else {
                    match second as char {
                        'A' => return EditorKey::SpecialKey(EditorSpecialKey::ArrowUp),
                        'B' => return EditorKey::SpecialKey(EditorSpecialKey::ArrowDown),
                        'C' => return EditorKey::SpecialKey(EditorSpecialKey::ArrowRight),
                        'D' => return EditorKey::SpecialKey(EditorSpecialKey::ArrowLeft),
                        'H' => return EditorKey::SpecialKey(EditorSpecialKey::HomeKey),
                        'F' => return EditorKey::SpecialKey(EditorSpecialKey::EndKey),
                        _ => {}
                    }
                }
            } else if first as char == 'O' {
                if second as char == 'H' {
                    return EditorKey::SpecialKey(EditorSpecialKey::HomeKey);
                } else if second as char == 'F' {
                    return EditorKey::SpecialKey(EditorSpecialKey::EndKey);
                }
            }

            return EditorKey::Key('\x1b');
        }

        // Backspace
        if c == 127 {
            return EditorKey::SpecialKey(EditorSpecialKey::Backspace);
        }
        EditorKey::Key(c as char)
    }


    fn editor_move_cursor(&mut self, key: EditorSpecialKey) {
        let mut row = None;
        if self.cy < self.num_rows() {
            row = Some(&self.rows[self.cy]);
        }

        match key {
            EditorSpecialKey::ArrowLeft => {
                if self.cx != 0 {
                    self.cx -= 1;
                } else if self.cy > 0 {
                    self.cy -= 1;
                    if self.cy < self.rows.len() {
                        self.cx = self.rows[self.cy].row.len();
                    }
                }
            }
            EditorSpecialKey::ArrowRight => {
                if row.is_some() && self.cx < row.unwrap().row.len() {
                    self.cx += 1;
                } else {
                    self.cy += 1;
                    self.cx = 0;
                }
            }
            EditorSpecialKey::ArrowUp if self.cy != 0 => self.cy -= 1,
            EditorSpecialKey::ArrowDown  if self.cy < self.num_rows() => self.cy += 1,
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
                    buff.extend(self
                        .rows[row_num].render[col_num..col_num + len].as_bytes());
                }
            }

            buff.extend("\x1b[K".as_bytes());
            buff.extend("\r\n".as_bytes());
        }
    }

    fn editor_draw_status_bar(&self, buff: &mut Vec<u8>) {
        buff.extend("\x1b[7m".as_bytes());
        let num_rows = self.rows.len();
        let status_bytes = format!("{:.20} - {} lines {}",
                                   if let Some(filename) = self.filename.as_ref()
                                   { filename } else { "[No Name]" }, num_rows,
                                   if self.dirty > 0 { "(modified)" } else { "" }).to_string();

        let mut len = min(status_bytes.len(), self.screen_cols);

        let line_info_bytes = format!("{}/{}", self.cy + 1, self.num_rows()).to_string();
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