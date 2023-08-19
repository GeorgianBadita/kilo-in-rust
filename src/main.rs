use std::env::args;
use std::io::stdin;
use std::os::fd::AsRawFd;
use std::process;

use kilo_in_rust::{Editor, erase_screen_for_exit, TermSettings};

fn run_editor(editor: &mut Editor) {
    editor.editor_set_status_message("HELP: CTRL-S = save | CTRL-Q = quit | CTRL-F = find");

    loop {
        editor.refresh_screen();
        if editor.editor_process_key_press() {
            break;
        }
    };
}


fn main() {
    let term_settings = TermSettings::from_fd(stdin().as_raw_fd());

    if let Err(e) = term_settings {
        erase_screen_for_exit();
        eprintln!("{}", e);
        process::exit(1);
    }

    let editor_res = Editor::from_settings(term_settings.unwrap());

    if let Err(e) = editor_res {
        erase_screen_for_exit();
        eprintln!("{}", e);
        process::exit(1);
    }

    let mut editor = editor_res.unwrap();

    let env_args = args().collect::<Vec<String>>();

    if env_args.len() >= 2 {
        editor.editor_open(&env_args[1]);
    }

    run_editor(&mut editor);
}