#![deny(unused_must_use, unsafe_op_in_unsafe_fn)]

use libc::{c_char, c_int, c_ushort};
use phf_shared::{PhfBorrow, PhfHash};
use std::convert::TryInto;
use std::ffi::{CStr, CString};
use std::hash::Hasher;
use std::mem::take;
use std::io::Error;
use std::ptr::NonNull;
use tinyvec::TinyVec;

// adapted from Kerrisk
// FIXME informative errors
fn create_pty() -> Result<(c_int, NonNull<c_char>), Error> {
    unsafe {
        let master_fd = match libc::posix_openpt(libc::O_RDWR | libc::O_NOCTTY) {
            -1 => return Err(Error::last_os_error()),
            fd => fd,
        };
        if libc::grantpt(master_fd) == -1 || libc::unlockpt(master_fd) == -1 {
            let e = Error::last_os_error();
            libc::close(master_fd);
            return Err(e);
        }
        let slave_name = NonNull::new(libc::ptsname(master_fd)).ok_or_else(|| {
            let e = Error::last_os_error();
            libc::close(master_fd);
            e
        })?;
        Ok((master_fd, slave_name))
    }
}

// adapted from Kerrisk
// FIXME informative errors
fn spawn_with_pty<'a>(
    cmd: &CStr,
    args: impl Iterator<Item = &'a CStr>,
    winsize: libc::winsize,
    termios: Option<libc::termios>,
) -> Result<(c_int, libc::pid_t), Error> {
    let (master_fd, slave_name) = create_pty()?;
    match unsafe { libc::fork() } {
        -1 => Err(Error::last_os_error()),
        0 => unsafe {
            if libc::setsid() == -1 {
                return Err(Error::last_os_error());
            }
            libc::close(master_fd);
            let slave_fd = match libc::open(slave_name.as_ptr(), libc::O_RDWR) {
                -1 => return Err(Error::last_os_error()),
                fd => fd,
            };

            if libc::ioctl(slave_fd, libc::TIOCSWINSZ, &winsize as *const _) == -1 {
                return Err(Error::last_os_error());
            }
            if let Some(termios) = termios {
                if libc::tcsetattr(slave_fd, libc::TCSANOW, &termios as *const _) == -1 {
                    return Err(Error::last_os_error());
                }
            }

            if libc::dup2(slave_fd, libc::STDIN_FILENO) != libc::STDIN_FILENO
                || libc::dup2(slave_fd, libc::STDOUT_FILENO) != libc::STDOUT_FILENO
                || libc::dup2(slave_fd, libc::STDERR_FILENO) != libc::STDERR_FILENO
            {
                return Err(Error::last_os_error());
            }
            assert!(slave_fd > libc::STDERR_FILENO);
            libc::close(slave_fd);

            let mut args: Vec<*const c_char> = args.map(|arg| arg.as_ptr()).collect();
            args.push(std::ptr::null());
            libc::execv(cmd.as_ptr(), args.as_ptr());
            Err(Error::last_os_error())
        },
        child_pid => Ok((master_fd, child_pid)),
    }
}

#[derive(Debug)]
pub struct TermBuilder {
    width: c_ushort,
    height: c_ushort,
    sh_path: CString,
    sh_args: Vec<CString>,
}

impl TermBuilder {
    pub fn width(mut self, w: u16) -> Self {
        self.width = w;
        self
    }

    pub fn height(mut self, h: u16) -> Self {
        self.height = h;
        self
    }

    pub fn arg(mut self, s: &str) -> Self {
        self.sh_args.push(CString::new(s.to_owned()).unwrap());
        self
    }

    pub fn spawn(self) -> Result<Term, Error> {
        let (pty, sh_pid) = spawn_with_pty(
            self.sh_path.as_ref(),
            self.sh_args.iter().map(AsRef::as_ref),
            libc::winsize {
                ws_row: self.height,
                ws_col: self.width,
                // these two fields are ignored
                ws_xpixel: 0,
                ws_ypixel: 0,
            },
            None,
        )?;
        Ok(Term {
            pty,
            sh_pid,
            state: State::new(self.width as usize, self.height as usize),
        })
    }
}

#[derive(Debug)]
pub struct Term {
    pty: c_int,
    sh_pid: libc::pid_t, // probably useful for something
    state: State,
}

impl Term {
    pub fn builder(sh_path: &str) -> TermBuilder {
        TermBuilder {
            width: 80,
            height: 50,
            sh_path: CString::new(sh_path.to_owned()).unwrap(),
            sh_args: Vec::new(),
        }
    }
}

impl Drop for Term {
    fn drop(&mut self) {
        // When we close this fd (which should be the only fd referring to the pty master), the sh
        // process we spawned will get SIGHUP.
        unsafe {
            libc::close(self.pty);
        }
    }
}

#[derive(Clone, Debug)]
struct State {
    screen: Screen,
    parsing: Parsing,
}

impl State {
    fn new(width: usize, height: usize) -> Self {
        Self {
            screen: Screen::new(width, height),
            parsing: Parsing::Start,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Param {
    D,
    N(u32),
}

impl Param {
    fn get_or(self, def: u32) -> u32 {
        match self {
            Self::D => def,
            Self::N(n) => n,
        }
    }
}

fn perform_csi_command(params: &[u8], intermeds: &[u8], fin: u8, screen: &mut Screen) {
    match (intermeds, fin) {
        (b"", b'A') => screen.cursor_up(the(parse_parameters(params)).get_or(1)),
        (b"", b'B') => screen.cursor_down(the(parse_parameters(params)).get_or(1)),
        (b"", b'C') => screen.cursor_forward(the(parse_parameters(params)).get_or(1)),
        (b"", b'D') => screen.cursor_back(the(parse_parameters(params)).get_or(1)),

        (b"", b'E') => screen.cursor_next_line(the(parse_parameters(params)).get_or(1)),
        (b"", b'F') => screen.cursor_prev_line(the(parse_parameters(params)).get_or(1)),
        (b"", b'G') => screen.cursor_horiz_abs(the(parse_parameters(params)).get_or(1)),

        (b"", b'm') => perform_sgr_command(parse_parameters(params), screen),

        _ => todo!(),
    }
}

fn perform_sgr_command(mut params: impl Iterator<Item = Param>, screen: &mut Screen) {
    while let Some(param) = params.next() {
        let n = param.get_or(0);
        match n {
            0 => screen.reset_all_attributes(),

            30..=37 => screen.set_fg_color(BasicColor::from_code(n - 30)),
            38 => {
                let rgb = parse_color(&mut params);
                screen.set_fg_color(rgb);
            }
            39 => screen.reset_fg_color(),

            40..=47 => screen.set_bg_color(BasicColor::from_code(n - 40)),
            48 => {
                let rgb = parse_color(&mut params);
                screen.set_bg_color(rgb);
            }
            49 => screen.reset_bg_color(),

            _ => todo!(),
        }
    }
}

fn parse_color(params: &mut impl Iterator<Item = Param>) -> Rgb {
    match params.next().unwrap() {
        Param::N(2) => {
            let r = params.next().unwrap().get_or(0).try_into().unwrap();
            let g = params.next().unwrap().get_or(0).try_into().unwrap();
            let b = params.next().unwrap().get_or(0).try_into().unwrap();
            Rgb { r, g, b }
        }
        Param::N(5) => {
            let x = params.next().unwrap().get_or(0).try_into().unwrap();
            LowColor(x).into()
        }
        _ => todo!(),
    }
}

fn parse_parameters(params: &[u8]) -> impl Iterator<Item = Param> + '_ {
    params.split(|&b| b == b';').map(|digits| {
        digits.iter().fold(Param::D, |acc, &d| {
            assert!((b'0'..=b'9').contains(&d));
            let n = acc.get_or(0) * 10 + d as u32 - b'0' as u32;
            Param::N(n)
        })
    })
}

fn the<T>(mut it: impl Iterator<Item = T>) -> T {
    let x = it.next().unwrap();
    assert!(it.next().is_none());
    x
}

#[derive(Clone, Copy, Debug)]
enum BasicColor {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
struct Rgb {
    r: u8,
    g: u8,
    b: u8,
}

impl From<BasicColor> for Rgb {
    fn from(col: BasicColor) -> Self {
        let (r, g, b) = match col {
            BasicColor::Black => (0, 0, 0),
            BasicColor::Red => (170, 0, 0),
            BasicColor::Green => (0, 170, 0),
            BasicColor::Yellow => (170, 85, 0),
            BasicColor::Blue => (0, 0, 170),
            BasicColor::Magenta => (170, 0, 170),
            BasicColor::Cyan => (0, 170, 170),
            BasicColor::White => (170, 170, 170),
        };
        Self { r, g, b }
    }
}

impl BasicColor {
    fn from_code(n: u32) -> Self {
        match n {
            0 => Self::Black,
            1 => Self::Red,
            2 => Self::Green,
            3 => Self::Yellow,
            4 => Self::Blue,
            5 => Self::Magenta,
            6 => Self::Cyan,
            7 => Self::White,
            _ => panic!(),
        }
    }
}

struct BrightColor(BasicColor);

impl From<BrightColor> for Rgb {
    fn from(col: BrightColor) -> Self {
        col.0.into()
    }
}

struct LowColor(u8);

// TODO
static LOW_COLOR_TABLE: [Rgb; 256] = [Rgb { r: 0, g: 0, b: 0 }; 256];

impl From<LowColor> for Rgb {
    fn from(col: LowColor) -> Self {
        LOW_COLOR_TABLE[col.0 as usize]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Glyph {
    Unknown,
    Ascii(u8),
}

impl PhfHash for Glyph {
    fn phf_hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Self::Unknown => state.write_u8(0),
            Self::Ascii(x) => {
                state.write_u8(1);
                state.write_u8(x);
            }
        }
    }
}

impl PhfBorrow<Glyph> for Glyph {
    fn borrow(&self) -> &Glyph {
        self
    }
}

#[derive(Clone, Copy, Debug)]
struct Tile {
    glyph: Glyph,
    fg_color: Rgb,
}

#[derive(Clone, Debug)]
struct Screen {
    width: usize,
    height: usize,
    tiles: Vec<Option<Tile>>,
    latest_fg_color: Rgb,
}

const DEFAULT_FG_COLOR: Rgb = Rgb {
    r: 0xff,
    g: 0xff,
    b: 0xff,
};
const DEFAULT_BG_COLOR: Rgb = Rgb {
    r: 0x00,
    g: 0x00,
    b: 0x00,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FontPixel {
    Background,
    Foreground,
}

struct Font {
    tile_width: usize,
    tile_height: usize,
    table: phf::Map<Glyph, &'static [&'static [FontPixel]]>,
}

impl Screen {
    fn new(width: usize, height: usize) -> Self {
        Self {
            width,
            height,
            tiles: vec![None; width * height],
            latest_fg_color: DEFAULT_FG_COLOR,
        }
    }

    fn put(&mut self, glyph: Glyph) {}

    fn backspace(&mut self) {}

    fn tab(&mut self) {}

    fn line_feed(&mut self) {}

    fn carriage_return(&mut self) {}

    fn cursor_up(&mut self, by: u32) {}

    fn cursor_down(&mut self, by: u32) {}

    fn cursor_forward(&mut self, by: u32) {}

    fn cursor_back(&mut self, by: u32) {}

    fn cursor_next_line(&mut self, by: u32) {}

    fn cursor_prev_line(&mut self, by: u32) {}

    fn cursor_horiz_abs(&mut self, col: u32) {}

    fn reset_all_attributes(&mut self) {
        self.reset_fg_color();
        self.reset_bg_color();
    }

    fn set_fg_color(&mut self, color: impl Into<Rgb>) {
        self.latest_fg_color = color.into();
    }

    fn reset_fg_color(&mut self) {
        self.latest_fg_color = DEFAULT_FG_COLOR;
    }

    fn set_bg_color(&mut self, color: impl Into<Rgb>) {}

    fn reset_bg_color(&mut self) {}

    fn rasterize(&self, font: &Font, pixels: &mut Vec<Rgb>) {
        pixels.clear();
        pixels.resize(
            self.width * font.tile_width * self.height * font.tile_height,
            DEFAULT_BG_COLOR,
        );

        for i in 0..self.height {
            for j in 0..self.width {
                // rasterize one tile
                let increment = self.width * font.tile_width;
                let mut base = i * font.tile_height * increment + j * font.tile_width;
                if let Some(tile) = self.tiles[i * self.width + j] {
                    let bitmap = font.table[&tile.glyph];
                    for &bitmap_row in bitmap {
                        for (pix, bit) in pixels[base..base + font.tile_width]
                            .iter_mut()
                            .zip(bitmap_row)
                        {
                            *pix = match *bit {
                                FontPixel::Foreground => tile.fg_color,
                                FontPixel::Background => DEFAULT_BG_COLOR,
                            }
                        }
                        base += increment;
                    }
                } else {
                    for _ in 0..font.tile_height {
                        pixels[base..base + font.tile_width].fill(DEFAULT_BG_COLOR);
                        base += increment;
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
enum Parsing {
    Start,
    Esc,

    CsiParameter(TinyVec<[u8; 16]>),
    CsiIntermediate(TinyVec<[u8; 16]>, TinyVec<[u8; 16]>),
}

impl Default for Parsing {
    fn default() -> Self {
        Self::Start
    }
}

impl Parsing {
    fn update(&mut self, byte: u8, screen: &mut Screen) {
        *self = match (&mut *self, byte) {
            (Parsing::Start, 0x08) => {
                screen.backspace();
                Parsing::Start
            }
            (Parsing::Start, b'\t') => {
                screen.tab();
                Parsing::Start
            }
            (Parsing::Start, b'\n') => {
                screen.line_feed();
                Parsing::Start
            }
            (Parsing::Start, b'\r') => {
                screen.carriage_return();
                Parsing::Start
            }
            (Parsing::Start, 0x1b) => Parsing::Esc,

            (Parsing::Esc, 0x9b) => Parsing::CsiParameter(TinyVec::new()),

            (Parsing::CsiParameter(params), b'0'..=b'?') => {
                params.push(byte);
                Parsing::CsiParameter(take(params))
            }
            (Parsing::CsiParameter(params), b'!'..=b'/') => {
                let mut intermeds = TinyVec::new();
                intermeds.push(byte);
                Parsing::CsiIntermediate(take(params), intermeds)
            }
            (Parsing::CsiParameter(params), b'@'..=b'~') => {
                perform_csi_command(params, &[], byte, screen);
                Parsing::Start
            }

            (Parsing::CsiIntermediate(params, intermeds), b'!'..=b'/') => {
                intermeds.push(byte);
                Parsing::CsiIntermediate(take(params), take(intermeds))
            }
            (Parsing::CsiIntermediate(params, intermeds), b'@'..=b'~') => {
                perform_csi_command(params, intermeds, byte, screen);
                Parsing::Start
            }

            _ => todo!(),
        };
    }
}
