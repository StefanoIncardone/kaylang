pub mod artifacts;
pub mod cli;
pub mod compiler;
pub mod src_file;
pub mod syntax;

use cli::{DirPath, FilePath};
use std::{fmt::{Display, Write as _}, io::IsTerminal, time::Instant};

#[derive(Debug, Default, Clone, Copy)]
pub enum Color {
    #[default]
    Auto,
    Always,
    Never,
}

#[derive(Debug, Default, Clone, Copy)]
pub enum Verbosity {
    #[default]
    Normal,
    Quiet,
    Verbose,
}

#[derive(Debug, Default, Clone)]
pub enum Command {
    #[default]
    Help,
    Version,
    Check {
        src_path: FilePath,
    },
    Compile {
        src_path: FilePath,
        out_path: Option<DirPath>,
    },
    Run {
        src_path: FilePath,
        out_path: Option<DirPath>,
    },
}

#[derive(Clone, Copy, Debug)]
pub struct Version {
    pub color: Color,
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.color.set(&std::io::stdout());
        return write!(f, "Kaylang compiler, version {VERSION}");
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Help {
    pub color: Color,
}

impl Display for Help {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            r"{version}

{USAGE}: kay [{OPTIONS}] [{COMMAND}]

{OPTIONS}:
    -c, --color <{MODE}>    Wether to display colored output ({MODE}: auto (default), never, always)
    -q, --quiet           Don't display any diagnostic messages
    -V, --verbose         Display extra diagnostic messages

{COMMAND}s:
    help,    -h, --help         Display this message (default)
    version, -v, --version      Display the compiler version
    check    <{FILE}>             Check the source code for correctness
    compile  <{FILE}> [{OUTPUT}]    Compile the source code down to an executable
    run      <{FILE}> [{OUTPUT}]    Compile and run the generated executable

{OUTPUT}:
    -o, --output <{PATH}>   Folder to populate with compilation artifacts (.asm, .o, executable) (default: '.')",
            version = Version { color: self.color }
        );
    }
}

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
    pub fn set<I: IsTerminal>(self, sink: &I) {
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
    return text.fmt(f);
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
        _ = write!(codes, "{};", fg as u8);
    }
    if bg != Bg::Default {
        _ = write!(codes, "{};", bg as u8);
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

    return if codes.is_empty() {
        text.fmt(f)
    } else {
        let _last_semicolon = codes.pop();

        write!(f, "\x1b[{codes}m")?;
        text.fmt(f)?;
        write!(f, "\x1b[0m")
    };
}

#[derive(Debug, Default)]
pub struct Colored<Text: AsRef<str>> {
    pub text: Text,
    pub fg: Fg,
    pub bg: Bg,
    pub flags: Flags,
}

impl<Text: AsRef<str>> Display for Colored<Text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return unsafe { log(self.text.as_ref(), self.fg, self.bg, self.flags, f) };
    }
}

// help and version messages
const HELP_FG: Fg = Fg::White;
const HELP_BG: Bg = Bg::Default;
const HELP_FLAGS: Flags = Flag::Bold;

#[rustfmt::skip] pub(crate) static VERSION: Colored<&str> = Colored { text: env!("CARGO_PKG_VERSION"), fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static USAGE:   Colored<&str> = Colored { text: "Usage",                   fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static OPTIONS: Colored<&str> = Colored { text: "Options",                 fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static COMMAND: Colored<&str> = Colored { text: "Command",                 fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static MODE:    Colored<&str> = Colored { text: "mode",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static FILE:    Colored<&str> = Colored { text: "file",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static PATH:    Colored<&str> = Colored { text: "path",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static OUTPUT:  Colored<&str> = Colored { text: "Output",                  fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };

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

// sub compilation steps (displayed when verbosity lever is verbose)
const SUBSTEP_FG: Fg = Fg::LightBlue;
const SUBSTEP_BG: Bg = Bg::Default;
const SUBSTEP_FLAGS: Flags = Flag::Bold;
const SUBSTEP_INDENT: usize = 4;
const SUBSTEP_PADDING: usize = 14;

#[rustfmt::skip] pub static LOADING_SOURCE: Colored<&str> = Colored { text: "Loding Source",  fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static TOKENIZATION:   Colored<&str> = Colored { text: "Tokenizing",     fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static BUILDING_AST:   Colored<&str> = Colored { text: "Building Ast",   fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static GENERATING_ASM: Colored<&str> = Colored { text: "Generating asm", fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static ASSEMBLING:     Colored<&str> = Colored { text: "Assembling",     fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static LINKING:        Colored<&str> = Colored { text: "Linking",        fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static SUBSTEP_DONE:   Colored<&str> = Colored { text: "Done",           fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };

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

// IDEA(stefano): this struct and color related features could be extracted to a separate crate
pub struct Step;

#[allow(clippy::print_stderr)]
impl Step {
    pub fn info(step: &Colored<&str>, path: &FilePath, verbosity: Verbosity) {
        if let Verbosity::Normal | Verbosity::Verbose = verbosity {
            eprintln!(
                "{spaces:STEP_INDENT$}{step:>STEP_PADDING$}: {path}",
                spaces = "",
                path = path.display()
            );
        }
    }

    fn done<Text: AsRef<str>>(
        start_time: Instant,
        step: &Colored<Text>,
        indent: usize,
        padding: usize,
    ) {
        let elapsed_time = Colored {
            text: format!("{}s", start_time.elapsed().as_secs_f32()),
            fg: Fg::White,
            ..Default::default()
        };

        eprintln!("{spaces:indent$}{step:>padding$}: in {elapsed_time}", spaces = "");
    }

    pub fn step_done(start_time: Instant, verbosity: Verbosity) {
        if let Verbosity::Normal | Verbosity::Verbose = verbosity {
            Self::done(start_time, &DONE, STEP_INDENT, STEP_PADDING);
        }
    }

    pub fn sub_step_done<Text: AsRef<str>>(
        start_time: Instant,
        sub_step: &Colored<Text>,
        verbosity: Verbosity,
    ) {
        if let Verbosity::Verbose = verbosity {
            Self::done(start_time, sub_step, SUBSTEP_INDENT, SUBSTEP_PADDING);
        }
    }
}
