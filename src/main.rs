use std::io::{Read, stdin, stdout, Write};
use std::os::fd::{AsFd, AsRawFd};

use nix::errno::Errno;
use nix::libc::{atexit, VMIN, VTIME};
use nix::sys::termios;
use nix::sys::termios::{ControlFlags, InputFlags, LocalFlags, OutputFlags, SetArg};
use rustix::termios::{tcgetwinsize, Winsize};

struct EditorConfig {
    screen_rows: usize,
    screen_cols: usize,
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
            screen_rows
        }
    }
}

fn get_window_size() -> Result<(usize, usize),  &'static str> {
    use rustix::termios::tcgetwinsize;

    //TODO: better error handling here
    let ioctl_res = tcgetwinsize(stdout().as_fd());
    if let Ok(r) = ioctl_res {
        return Ok((r.ws_col as usize, r.ws_row as usize));
    }

    stdout().write("\x1b[999C\x1b[999B".as_bytes()).unwrap();
    stdout().flush().unwrap();
    editor_read_key();
    return Err("some");
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


fn editor_draw_rows(editor_config: &EditorConfig) {
    for y in 0..=editor_config.screen_rows {
        stdout().write("~\r\n".as_bytes()).unwrap();
    }
    stdout().flush().unwrap();
}

fn ctrl_key(k: char) -> u8 {
    return (k as u8) & 0x1f;
}

fn refresh_screen(editor_config: &EditorConfig) {
    stdout().write("\x1b[2J".as_bytes()).unwrap();
    stdout().write("\x1b[H".as_bytes()).unwrap();

    editor_draw_rows(editor_config);

    stdout().write("\x1b[H".as_bytes()).unwrap();
    stdout().flush().unwrap();
}

fn editor_read_key() -> char {
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
    return c;
}

fn editor_process_key_press() {
    let c = editor_read_key();

    if ctrl_key('q') == c as u8 {
        stdout().write("\x1b[2J".as_bytes()).unwrap();
        stdout().write("\x1b[H".as_bytes()).unwrap();
        stdout().flush().unwrap();

        std::process::exit(0);
    }
}

fn run_editor(editor_config: &EditorConfig) {
    loop {
        refresh_screen(editor_config);
        editor_process_key_press();
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

    let editor_config = EditorConfig::new();

    run_editor(&editor_config);


    Ok(())
}
