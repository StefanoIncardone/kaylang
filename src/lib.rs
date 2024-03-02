pub mod assembler;
pub mod ast;
pub mod cli;
pub mod color;
pub mod compiler;
pub mod error;
pub mod linker;
pub mod logging;
pub mod run;
pub mod src_file;
pub mod tokenizer;

use color::{Bg, Colored, Fg, Flag, Flags};
use std::{fmt::Display, path::PathBuf};

// help and version messages
const HELP_FG: Fg = Fg::White;
const HELP_BG: Bg = Bg::Default;
const HELP_FLAGS: Flags = Flag::Bold;

#[rustfmt::skip] pub(crate) static VERSION:  Colored<&str> = Colored { text: env!("CARGO_PKG_VERSION"), fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static OPTIONS:  Colored<&str> = Colored { text: "Options",                 fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static RUN_MODE: Colored<&str> = Colored { text: "Run mode",                fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static MODE:     Colored<&str> = Colored { text: "mode",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static FILE:     Colored<&str> = Colored { text: "file",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static PATH:     Colored<&str> = Colored { text: "path",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static OUTPUT:   Colored<&str> = Colored { text: "Output",                  fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };

#[derive(Clone, Copy, Debug)]
pub struct Version {
    pub color: Color,
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.color.set(&std::io::stdout());
        write!(f, "Kaylang compiler, version {VERSION}")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Help {
    pub color: Color,
}

impl Display for Help {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r"{version}

Usage: kay [{OPTIONS}] [{RUN_MODE}]

{OPTIONS}:
-h, --help            Display this message (selected when no other run commands are provided)
-v, --version         Display the compiler version
-c, --color <{MODE}>    Wether to display colored output ({MODE}: auto (default), never, always)
-q, --quiet           Don't display any diagnostic messages
-V, --verbose         Display extra diagnostic messages

{RUN_MODE}:
check    <{FILE}>              Check the source code for correctness
compile  <{FILE}> [{OUTPUT}]     Compile the source code down to an executable
run      <{FILE}> [{OUTPUT}]     Compile and run the generated executable

{OUTPUT}:
-o, --output <{PATH}>       Folder to populate with compilation artifacts (.asm, .o, executable) (default: '.')",
            version = Version { color: self.color }
        )
    }
}

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
pub enum RunMode {
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
