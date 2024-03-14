use crate::{cli::Utf8Path, Color, Verbosity};
use std::{fmt::Display, io::IsTerminal, time::Instant};

#[allow(dead_code)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Fg {
    #[default]
    Default = 0,
    Black = 30,
    Red = 31,
    Green = 32,
    Yellow = 33,
    Blue = 34,
    Magenta = 35,
    Cyan = 36,
    LightGray = 37,
    DarkGray = 90,
    LightRed = 91,
    LightGreen = 92,
    LightYellow = 93,
    LightBlue = 94,
    LightMagenta = 95,
    LightCyan = 96,
    White = 97,
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Bg {
    #[default]
    Default = 0,
    Black = 40,
    DarkRed = 41,
    DarkGreen = 42,
    DarkYellow = 43,
    DarkBlue = 44,
    DarkMagenta = 45,
    DarkCyan = 46,
    DarkWhite = 47,
    BrightBlack = 100,
    BrightRed = 101,
    BrightGreen = 102,
    BrightYellow = 103,
    BrightBlue = 104,
    BrightMagenta = 105,
    BrightCyan = 106,
    White = 107,
}

pub type Flags = u8;
pub struct Flag;

#[allow(non_upper_case_globals)]
#[allow(dead_code)]
impl Flag {
    pub const Default: Flags = 0b0000_0000;
    pub const Bold: Flags = 0b0000_0001;
    pub const Underline: Flags = 0b0000_0010;
    pub const NoUnderline: Flags = 0b0000_0100;
    pub const ReverseText: Flags = 0b0000_1000;
    pub const PositiveText: Flags = 0b0001_0000;
}

#[allow(non_upper_case_globals)]
pub(crate) static mut log: fn(
    &str,
    Fg,
    Bg,
    Flags,
    &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result = log_color;

impl Color {
    pub fn set(self, sink: &impl IsTerminal) {
        unsafe {
            log = match self {
                Self::Auto => {
                    if sink.is_terminal() {
                        log_color
                    } else {
                        log_no_color
                    }
                }
                Self::Always => log_color,
                Self::Never => log_no_color,
            }
        }
    }
}

fn log_no_color(
    text: &str,
    _: Fg,
    _: Bg,
    _: Flags,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    text.fmt(f)
}

fn log_color(
    text: &str,
    fg: Fg,
    bg: Bg,
    flags: Flags,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    let mut codes = String::with_capacity(15);

    if fg != Fg::Default {
        codes += &format!("{fg};", fg = fg as u8);
    }
    if bg != Bg::Default {
        codes += &format!("{bg};", bg = bg as u8);
    }
    if flags & Flag::Bold != 0 {
        codes += "1;";
    }
    if flags & Flag::Underline != 0 {
        codes += "4;";
    }
    if flags & Flag::NoUnderline != 0 {
        codes += "24;";
    }
    if flags & Flag::ReverseText != 0 {
        codes += "7;";
    }
    if flags & Flag::PositiveText != 0 {
        codes += "27;";
    }

    if codes.is_empty() {
        text.fmt(f)
    } else {
        let _last_semicolon = codes.pop();

        write!(f, "\x1b[{codes}m")?;
        text.fmt(f)?;
        write!(f, "\x1b[0m")
    }
}

#[derive(Debug, Default)]
pub struct Colored<Str: AsRef<str>> {
    pub text: Str,
    pub fg: Fg,
    pub bg: Bg,
    pub flags: Flags,
}

impl<Str: AsRef<str>> Display for Colored<Str> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { log(self.text.as_ref(), self.fg, self.bg, self.flags, f) }
    }
}

// errors
const ERR_FG: Fg = Fg::LightRed;
const ERR_BG: Bg = Bg::Default;
const ERR_FLAGS: Flags = Flag::Bold;

const BAR_FG: Fg = Fg::LightBlue;
const BAR_BG: Bg = Bg::Default;
const BAR_FLAGS: Flags = Flag::Bold;

#[rustfmt::skip] pub(crate) static ERROR: Colored<&str> = Colored { text: "Error", fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static CAUSE: Colored<&str> = Colored { text: "Cause", fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static AT:    Colored<&str> = Colored { text: "at",    fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static BAR:   Colored<&str> = Colored { text: "|",     fg: BAR_FG, bg: BAR_BG, flags: BAR_FLAGS };

fn done(start_time: Instant, step: &Colored<&str>, indent: usize, padding: usize) {
    let elapsed_time = Colored {
        text: format!("{elapsed}s", elapsed = start_time.elapsed().as_secs_f32()),
        fg: Fg::White,
        ..Default::default()
    };

    eprintln!("{:indent$}{:>padding$}: in {}", "", step, elapsed_time);
}

// main compilation steps (displayed when verbosity level is normal or verbose)
const STEP_FG: Fg = Fg::LightGreen;
const STEP_BG: Bg = Bg::Default;
const STEP_FLAGS: Flags = Flag::Bold;
const STEP_INDENT: usize = 0;
const STEP_PADDING: usize = 9;

#[rustfmt::skip] pub static CHECKING:  Colored<&str> = Colored { text: "Checking",  fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static COMPILING: Colored<&str> = Colored { text: "Compiling", fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static RUNNING:   Colored<&str> = Colored { text: "Running",   fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static DONE:      Colored<&str> = Colored { text: "Done",      fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };

pub struct Step {
    pub start_time: Instant,
    pub verbosity: Verbosity,
}

impl Step {
    pub fn info(step: &Colored<&str>, path: &Utf8Path, verbosity: Verbosity) {
        if let Verbosity::Quiet = verbosity {
            return;
        }

        eprintln!("{:STEP_INDENT$}{step:>STEP_PADDING$}: {path}", "", path = path.inner.display());
    }

    pub fn done(self) {
        if let Verbosity::Quiet = self.verbosity {
            return;
        }

        done(self.start_time, &DONE, STEP_INDENT, STEP_PADDING);
    }
}

// sub compilation steps (displayed when verbosity lever is verbose)
const SUBSTEP_FG: Fg = Fg::LightBlue;
const SUBSTEP_BG: Bg = Bg::Default;
const SUBSTEP_FLAGS: Flags = Flag::Bold;
const SUBSTEP_INDENT: usize = 4;
const SUBSTEP_PADDING: usize = 14;

#[rustfmt::skip] pub static LOADING_SOURCE: Colored<&str> = Colored { text: "Loding Source",  fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static TOKENIZATION:   Colored<&str> = Colored { text: "Tokenizing",     fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static AST_BUILDING:   Colored<&str> = Colored { text: "Ast building",   fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static ASM_GENERATION: Colored<&str> = Colored { text: "Asm Generation", fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static ASSEMBLER:      Colored<&str> = Colored { text: "Assembler",      fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static LINKER:         Colored<&str> = Colored { text: "Linker",         fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static SUBSTEP_DONE:   Colored<&str> = Colored { text: "Done",           fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };

pub struct SubStep {
    pub step: &'static Colored<&'static str>,
    pub start_time: Instant,
    pub verbosity: Verbosity,
}

impl SubStep {
    pub fn done(self) {
        if let Verbosity::Quiet | Verbosity::Normal = self.verbosity {
            return;
        }

        done(self.start_time, self.step, SUBSTEP_INDENT, SUBSTEP_PADDING);
    }
}
