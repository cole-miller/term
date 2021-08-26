#![deny(unsafe_op_in_unsafe_fn)]

use libc::pid_t;
use std::ffi::{CStr, CString};
use std::mem::take;
use std::os::{raw::c_char, unix::io::RawFd};
use std::ptr::NonNull;
use tinyvec::TinyVec;

// adapted from Kerrisk
// FIXME informative errors
fn create_pty() -> Result<(RawFd, NonNull<c_char>), ()> {
    unsafe {
        let master_fd = match libc::posix_openpt(libc::O_RDWR | libc::O_NOCTTY) {
            -1 => return Err(()),
            fd => fd,
        };
        if libc::grantpt(master_fd) == -1 || libc::unlockpt(master_fd) == -1 {
            libc::close(master_fd);
            return Err(());
        }
        match NonNull::new(libc::ptsname(master_fd)) {
            None => {
                libc::close(master_fd);
                Err(())
            }
            Some(slave_name) => Ok((master_fd, slave_name)),
        }
    }
}

// adapted from Kerrisk
// FIXME informative errors
fn spawn_with_pty<'a>(
    cmd: &CStr,
    args: impl Iterator<Item = &'a CStr>,
    // termios: Option<libc::termios>,
    // winsize: Option<libc::winsize>,
) -> Result<(RawFd, pid_t), ()> {
    unsafe {
        let (master_fd, slave_name) = create_pty()?;
        match libc::fork() {
            -1 => Err(()),
            0 => {
                if libc::setsid() == -1 {
                    return Err(());
                }
                libc::close(master_fd);
                let slave_fd = match libc::open(slave_name.as_ptr(), libc::O_RDWR) {
                    -1 => return Err(()),
                    fd => fd,
                };

                // if let Some(termios) = termios {
                //     if libc::tcsetattr(slave_fd, libc::TCSANOW, &termios as *const _) == -1 {
                //         return Err(());
                //     }
                // }
                // if let Some(winsize) = winsize {
                //     if libc::ioctl(slave_fd, libc::TIOCSWINSZ, &winsize as *const _) == -1 {
                //         return Err(());
                //     }
                // }

                if libc::dup2(slave_fd, libc::STDIN_FILENO) != libc::STDIN_FILENO
                    || libc::dup2(slave_fd, libc::STDOUT_FILENO) != libc::STDOUT_FILENO
                    || libc::dup2(slave_fd, libc::STDERR_FILENO) != libc::STDERR_FILENO
                {
                    return Err(());
                }
                assert!(slave_fd > libc::STDERR_FILENO);
                libc::close(slave_fd);

                let mut args: Vec<*const c_char> = args.map(|arg| arg.as_ptr()).collect();
                args.push(std::ptr::null());
                libc::execv(cmd.as_ptr(), args.as_ptr());
                Err(())
            }
            child_pid => Ok((master_fd, child_pid)),
        }
    }
}

#[derive(Debug)]
pub struct TermBuilder {
    sh_path: CString,
    sh_args: Vec<CString>,
}

impl TermBuilder {
    pub fn spawn(self) -> Result<Term, ()> {
        let (pty, sh_pid) = spawn_with_pty(
            self.sh_path.as_ref(),
            self.sh_args.iter().map(AsRef::as_ref),
        )?;
        Ok(Term {
            pty,
            sh_pid,
            state: State::new(),
        })
    }
}

#[derive(Debug)]
pub struct Term {
    pty: RawFd,
    sh_pid: pid_t,
    state: State,
}

#[derive(Clone, Debug)]
struct State {
    screen: Screen,
    parsing: Parsing,
}

impl State {
    fn new() -> Self {
        Self {
            screen: Screen::new(),
            parsing: Parsing::Start,
        }
    }

    fn update(&mut self, byte: u8) {
        match (&mut self.parsing, byte) {
            // (Parsing::Start, 0x07) => return Some(Effect::Bell),
            (Parsing::Start, 0x08) => self.screen.backspace(),
            (Parsing::Start, b'\t') => self.screen.tab(),
            (Parsing::Start, b'\n') => self.screen.line_feed(),
            (Parsing::Start, b'\r') => self.screen.carriage_return(),
            (Parsing::Start, 0x1b) => self.parsing = Parsing::Esc,

            (Parsing::Esc, 0x9b) => self.parsing = Parsing::CsiParameter(TinyVec::new()),

            (Parsing::CsiParameter(params), b'0'..=b'?') => params.push(byte),
            (Parsing::CsiParameter(params), b'!'..=b'/') => {
                let mut intermeds = TinyVec::new();
                intermeds.push(byte);
                self.parsing = Parsing::CsiIntermediate(take(params), intermeds);
            }
            (Parsing::CsiParameter(params), b'@'..=b'~') => {
                perform_csi_command(params, &[], byte, &mut self.screen);
                self.parsing = Parsing::Start;
            }

            (Parsing::CsiIntermediate(_, intermeds), b'!'..=b'/') => intermeds.push(byte),
            (Parsing::CsiIntermediate(params, intermeds), b'@'..=b'~') => {
                perform_csi_command(params, intermeds, byte, &mut self.screen);
                self.parsing = Parsing::Start;
            }

            _ => todo!(),
        }
    }
}

fn perform_csi_command(params: &[u8], intermeds: &[u8], fin: u8, screen: &mut Screen) {
    match (params, intermeds, fin) {
        (_, b"", b'A') => screen.cursor_up(parse_nonzero(params)),
        (_, b"", b'B') => screen.cursor_down(parse_nonzero(params)),
        (_, b"", b'C') => screen.cursor_forward(parse_nonzero(params)),
        (_, b"", b'D') => screen.cursor_back(parse_nonzero(params)),

        _ => todo!(),
    }
}

fn parse_nonzero(params: &[u8]) -> u32 {
    todo!()
}

#[derive(Clone, Debug)]
struct Screen {}

impl Screen {
    fn new() -> Self {
        todo!()
    }

    fn backspace(&mut self) {}

    fn tab(&mut self) {}

    fn line_feed(&mut self) {}

    fn carriage_return(&mut self) {}

    fn cursor_up(&mut self, by: u32) {}

    fn cursor_down(&mut self, by: u32) {}

    fn cursor_forward(&mut self, by: u32) {}

    fn cursor_back(&mut self, by: u32) {}
}

#[derive(Clone, Debug)]
enum Parsing {
    Start,
    Esc,

    CsiParameter(TinyVec<[u8; 128]>),
    CsiIntermediate(TinyVec<[u8; 128]>, TinyVec<[u8; 128]>),
}
