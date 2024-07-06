pub mod artifacts;
pub mod cli;
pub mod compiler;
pub mod src_file;
pub mod syntax;
pub mod color;

use color::{Bg, Colored, Fg, Flag, Flags};
use std::{fmt::Display, path::{Path, PathBuf}, time::Instant};


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
        src_path: PathBuf,
    },
    Compile {
        src_path: PathBuf,
        out_path: Option<PathBuf>,
    },
    Run {
        src_path: PathBuf,
        out_path: Option<PathBuf>,
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
    pub fn info(step: &Colored<&str>, path: &Path, verbosity: Verbosity) {
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
