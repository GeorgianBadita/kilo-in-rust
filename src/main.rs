use std::cmp::{max, min};
use std::env::args;
use std::fs;
use std::io::{BufRead, BufReader, Read, stdin, stdout, Write};
use std::os::fd::{AsFd, AsRawFd};

use nix::errno::Errno;
use nix::libc::{atexit, VMIN, VTIME};
use nix::sys::termios;
use nix::sys::termios::{ControlFlags, InputFlags, LocalFlags, OutputFlags, SetArg};
use scanf::sscanf;

const KILO_VERSION: &str = "0.0.1";

#[derive(Debug, PartialEq)]
enum EditorSpecialKey {
    ArrowLeft,
    ArrowRight,
    ArrowUp,
    ArrowDown,
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
    render: String
}

impl Row {
    fn from_string(row: String) -> Row {
        Row {
            row: row.clone(),
            render: row
        }
    }
}

struct EditorConfig {
    screen_rows: i32,
    screen_cols: i32,
    cx: i32,
    cy: i32,
    rows: Vec<Row>,
    row_off: i32,
    col_off: i32,
}

impl EditorConfig {
    fn new() -> Self {
        let win_size_res = get_window_size();
        if let Err(err) = win_size_res {
            die(err);
        }

        let (screen_cols, screen_rows) = win_size_res.unwrap();
        EditorConfig {
            screen_cols,
            screen_rows,
            cx: 0,
            cy: 0,
            rows: Vec::new(),
            row_off: 0,
            col_off: 0,
        }
    }

    fn num_rows(&self) -> i32 {
        self.rows.len() as i32
    }

    fn editor_open(&mut self, file_path: &str) {
        let file = fs::File::open(file_path).unwrap_or_else(|e| panic!("{}", e));
        let buff = BufReader::new(file);
        buff.lines()
            .filter(|r| r.is_ok())
            .map(|r| r.unwrap().trim_end().to_string())
            .for_each(|line| self.editor_append_row(line));
    }

    fn editor_scroll(&mut self) {
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
    }

    fn editor_append_row(&mut self, row: String) {
        self.rows.push(Row::from_string(row));
    }
}

fn get_cursor_position() -> Result<(i32, i32), &'static str> {
    stdout().write("\x1b[6n".as_bytes()).unwrap();
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

    if let Err(_) = scanf_ret {
        return Err("Error parsing cursor position from scanf");
    }
    return Ok((cols, rows));
}

fn get_window_size() -> Result<(i32, i32), &'static str> {
    //TODO: better error handling here
    let ioctl_res = rustix::termios::tcgetwinsize(stdout().as_fd());
    if let Ok(r) = ioctl_res {
        return Ok((r.ws_col as i32, r.ws_row as i32));
    }

    // Go down and right 999 cols
    stdout().write("\x1b[999C\x1b[999B".as_bytes()).unwrap();
    stdout().flush().unwrap();

    return get_cursor_position();
}


fn die(message: &str) {
    stdout().write("\x1b[2J".as_bytes()).unwrap();
    stdout().write("\x1b[H".as_bytes()).unwrap();
    stdout().flush().unwrap();

    eprintln!("{}", message);
    std::process::exit(1)
}

// TODO: I would really love to do this another way :(
// Contemplate using some global object to store the original terminal state
#[no_mangle]
pub extern "C" fn disable_raw_mode() {
    let mut terminal_attrs = termios::tcgetattr(stdin().as_raw_fd()).unwrap();

    // Input flags
    terminal_attrs.input_flags.set(InputFlags::IXON, true);
    terminal_attrs.input_flags.set(InputFlags::ICRNL, true);

    // Output flags
    terminal_attrs.output_flags.set(OutputFlags::OPOST, true);

    // Local flags
    terminal_attrs.local_flags.set(LocalFlags::ECHO, true);
    terminal_attrs.local_flags.set(LocalFlags::ICANON, true);
    terminal_attrs.local_flags.set(LocalFlags::ISIG, true);
    terminal_attrs.local_flags.set(LocalFlags::IEXTEN, true);

    // Control flags
    terminal_attrs.control_flags.set(ControlFlags::CS8, true);

    // Set control chars
    terminal_attrs.control_chars[VMIN] = 0;
    terminal_attrs.control_chars[VTIME] = 1;

    // Applying new terminal settings
    termios::tcsetattr(stdin().as_raw_fd(), SetArg::TCSAFLUSH, &terminal_attrs).unwrap();
}

fn enable_raw_mode() -> Result<(), nix::errno::Errno> {
    let mut terminal_attrs = termios::tcgetattr(stdin().as_raw_fd())?;

    // Input flags
    terminal_attrs.input_flags.set(InputFlags::IXON, false);
    terminal_attrs.input_flags.set(InputFlags::ICRNL, false);
    terminal_attrs.input_flags.set(InputFlags::INPCK, false);
    terminal_attrs.input_flags.set(InputFlags::ISTRIP, false);
    terminal_attrs.input_flags.set(InputFlags::BRKINT, false);

    // Output flags
    terminal_attrs.output_flags.set(OutputFlags::OPOST, false);

    // Local flags
    terminal_attrs.local_flags.set(LocalFlags::ECHO, false);
    terminal_attrs.local_flags.set(LocalFlags::ICANON, false);
    terminal_attrs.local_flags.set(LocalFlags::ISIG, false);
    terminal_attrs.local_flags.set(LocalFlags::IEXTEN, false);

    // Control flags
    terminal_attrs.control_flags.set(ControlFlags::CS8, true);

    // Set control chars
    terminal_attrs.control_chars[VMIN] = 0;
    terminal_attrs.control_chars[VTIME] = 1;

    // Applying new terminal settings
    termios::tcsetattr(stdin().as_raw_fd(), SetArg::TCSAFLUSH, &terminal_attrs)?;

    return Ok(());
}


fn editor_draw_rows(editor_config: &EditorConfig, buff: &mut Vec<u8>) {
    for y in 0..editor_config.screen_rows {
        let filerow = y + editor_config.row_off;
        if filerow >= editor_config.num_rows() {
            if editor_config.num_rows() == 0 && y == editor_config.screen_rows / 3 {
                let mut welcome_message = String::from("Kilo editor -- version ") + KILO_VERSION;
                if welcome_message.len() as i32 > editor_config.screen_cols {
                    welcome_message = welcome_message[..editor_config.screen_cols as usize].to_string();
                }
                let mut padding = (editor_config.screen_cols - welcome_message.len() as i32) / 2;
                if padding > 0 {
                    buff.push('~' as u8);
                    padding -= 1;
                }
                buff.extend(" ".repeat(padding as usize).as_bytes());
                buff.extend(welcome_message.as_bytes());
            } else {
                buff.push('~' as u8);
            }
        } else {
            let len = min(
                max(editor_config.rows[filerow as usize].render.len() as i32 - editor_config.col_off, 0),
                editor_config.screen_cols) as usize;
            let row_num = filerow as usize;
            let col_num = editor_config.col_off as usize;
            if len != 0 {
                buff.extend(editor_config
                    .rows[row_num].render[col_num..col_num + len].as_bytes());
            }
        }

        buff.extend("\x1b[K".as_bytes());
        if y < editor_config.screen_rows - 1 {
            buff.extend("\r\n".as_bytes());
        }
    }
}

fn ctrl_key(k: char) -> char {
    return ((k as u8) & 0x1f) as char;
}

fn editor_move_cursor(editor_config: &mut EditorConfig, key: EditorSpecialKey) {
    let mut row = None;
    if editor_config.cy < editor_config.num_rows() {
        row = Some(&editor_config.rows[editor_config.cy as usize]);
    }

    match key {
        EditorSpecialKey::ArrowLeft => {
            if editor_config.cx != 0 {
                editor_config.cx -= 1;
            } else if editor_config.cy > 0 {
                editor_config.cy -= 1;
                editor_config.cx = editor_config.rows[editor_config.cy as usize].row.len() as i32;
            }
        }
        EditorSpecialKey::ArrowRight => {
            if row.is_some() && editor_config.cx < row.unwrap().row.len() as i32 {
                editor_config.cx += 1;
            } else {
                editor_config.cy += 1;
                editor_config.cx = 0;
            }
        }
        EditorSpecialKey::ArrowUp if editor_config.cy != 0 => editor_config.cy -= 1,
        EditorSpecialKey::ArrowDown  if editor_config.cy < editor_config.num_rows() => editor_config.cy += 1,
        _ => {}
    }

    let len = if editor_config.cy >= editor_config.num_rows()
    { 0 } else { editor_config.rows[editor_config.cy as usize].row.len() };
    if editor_config.cx > len as i32 {
        editor_config.cx = len as i32;
    }
}

fn refresh_screen(editor_config: &mut EditorConfig) {
    editor_config.editor_scroll();

    let mut buff: Vec<u8> = Vec::new();
    buff.extend("\x1b[?25l".as_bytes());
    buff.extend("\x1b[H".as_bytes());

    editor_draw_rows(editor_config, &mut buff);
    buff.extend(format!("\x1b[{};{}H",
                        (editor_config.cy - editor_config.row_off) + 1,
                        (editor_config.cx - editor_config.col_off) + 1).as_bytes());

    buff.extend("\x1b[?25h".as_bytes());

    stdout().write(&buff).unwrap();
    stdout().flush().unwrap();
}

fn editor_read_key() -> EditorKey {
    let mut buff: [u8; 1] = [0];
    let mut sdin = stdin();
    let c;
    loop {
        let read_result = sdin.read(&mut buff[..]);
        if read_result.is_err() {
            die("Error on read");
        }
        if read_result.unwrap() == 1 {
            c = buff[0] as char;
            break;
        }
    }

    if c == '\x1b' {
        let first_opt = stdin().bytes().next().and_then(|r| r.ok());
        let second_opt = stdin().bytes().next().and_then(|r| r.ok());

        if first_opt.is_none() || second_opt.is_none() {
            return EditorKey::Key('\x1b');
        }
        let first = first_opt.unwrap();
        let second = second_opt.unwrap();

        if first as char == '[' {
            if second as char >= '0' && second as char <= '9' {
                let third = stdin().bytes().next().and_then(|r| r.ok()).unwrap();
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
    return EditorKey::Key(c);
}

fn editor_process_key_press(editor_config: &mut EditorConfig) {
    let c = editor_read_key();
    match c {
        EditorKey::SpecialKey(EditorSpecialKey::HomeKey) => {
            editor_config.cx = 0;
        }
        EditorKey::SpecialKey(EditorSpecialKey::EndKey) => {
            editor_config.cx = editor_config.screen_cols - 1;
        }
        EditorKey::SpecialKey(EditorSpecialKey::PageUp) | EditorKey::SpecialKey(EditorSpecialKey::PageDown) => {
            let mut times = editor_config.screen_rows;
            while times > 0 {
                times -= 1;
                editor_move_cursor(editor_config, if c == EditorKey::SpecialKey(EditorSpecialKey::PageUp)
                { EditorSpecialKey::ArrowUp } else { EditorSpecialKey::ArrowDown })
            }
        }
        EditorKey::SpecialKey(key) => editor_move_cursor(editor_config, key),
        EditorKey::Key('a') => editor_move_cursor(editor_config, EditorSpecialKey::ArrowLeft),
        EditorKey::Key('d') => editor_move_cursor(editor_config, EditorSpecialKey::ArrowRight),
        EditorKey::Key('w') => editor_move_cursor(editor_config, EditorSpecialKey::ArrowUp),
        EditorKey::Key('s') => editor_move_cursor(editor_config, EditorSpecialKey::ArrowDown),
        EditorKey::Key(c) => {
            if ctrl_key('q') == c {
                stdout().write("\x1b[2J".as_bytes()).unwrap();
                stdout().write("\x1b[H".as_bytes()).unwrap();
                stdout().flush().unwrap();
                std::process::exit(0);
            }
        }
        _ => {}
    }
}

fn run_editor(editor_config: &mut EditorConfig) {
    loop {
        refresh_screen(editor_config);
        editor_process_key_press(editor_config);
    };
}


fn main() -> Result<(), Errno> {
    let en_raw_st = enable_raw_mode();
    unsafe {
        atexit(disable_raw_mode);
    }

    if let Err(e) = en_raw_st {
        die(&e.to_string());
    }

    let mut editor_config = EditorConfig::new();
    let env_args = args().collect::<Vec<String>>();

    if env_args.len() >= 2 {
        editor_config.editor_open(&env_args[1]);
    }

    run_editor(&mut editor_config);

    Ok(())
}
