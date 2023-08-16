use std::io;
use std::os::fd::RawFd;
use termios::*;

#[derive(Clone)]
pub struct TermSettings(Termios);

impl TermSettings {
    pub fn from_fd(fd: RawFd) -> io::Result<TermSettings> {
        let term = TermSettings(Termios::from_fd(fd)?);
        term.enable_raw_mode()?;
        return Ok(term);
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
        tcsetattr(libc::STDIN_FILENO, TCSANOW, &self.0).unwrap();
    }
}
