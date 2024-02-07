use std::{path::Path, time::Instant};

use crate::{
    color::{Bg, Colored, ColoredStr, Fg, Flag, Options},
    Verbosity,
};

// main compilation steps (displayed when verbosity level is normal or verbose)
const STEP_OPT: Options = Options { fg: Fg::LightGreen, bg: Bg::Default, flags: Flag::Bold };
pub(crate) const STEP_INDENT: usize = 0;
pub(crate) const STEP_PADDING: usize = 9;

pub(crate) static CHECKING: ColoredStr = ColoredStr { text: "Checking", opt: STEP_OPT };
pub(crate) static COMPILING: ColoredStr = ColoredStr { text: "Compiling", opt: STEP_OPT };
pub(crate) static RUNNING: ColoredStr = ColoredStr { text: "Running", opt: STEP_OPT };
pub(crate) static DONE: ColoredStr = ColoredStr { text: "Done", opt: STEP_OPT };

// sub compilation steps (displayed when verbosity lever is verbose)
const SUBSTEP_OPT: Options = Options { fg: Fg::LightBlue, bg: Bg::Default, flags: Flag::Bold };
pub(crate) const SUBSTEP_INDENT: usize = 4;
pub(crate) const SUBSTEP_PADDING: usize = 14;

pub(crate) static LOADING_SOURCE: ColoredStr = ColoredStr { text: "Loding Source", opt: SUBSTEP_OPT };
pub(crate) static LEXING: ColoredStr = ColoredStr { text: "Lexing", opt: SUBSTEP_OPT };
pub(crate) static AST_BUILDING: ColoredStr = ColoredStr { text: "Ast building", opt: SUBSTEP_OPT };
pub(crate) static ASM_GENERATION: ColoredStr = ColoredStr { text: "Asm Generation", opt: SUBSTEP_OPT };
pub(crate) static ASSEMBLER: ColoredStr = ColoredStr { text: "Assembler", opt: SUBSTEP_OPT };
pub(crate) static LINKER: ColoredStr = ColoredStr { text: "Linker", opt: SUBSTEP_OPT };
pub(crate) static SUBSTEP_DONE: ColoredStr = ColoredStr { text: "Done", opt: SUBSTEP_OPT };

// errors
const ERR_OPT: Options = Options { fg: Fg::LightRed, bg: Bg::Default, flags: Flag::Bold };
const BAR_OPT: Options = Options { fg: Fg::LightBlue, bg: Bg::Default, flags: Flag::Bold };

pub(crate) static ERROR: ColoredStr = ColoredStr { text: "Error", opt: ERR_OPT };
pub(crate) static CAUSE: ColoredStr = ColoredStr { text: "Cause", opt: ERR_OPT };
pub(crate) static AT: ColoredStr = ColoredStr { text: "at", opt: ERR_OPT };
pub(crate) static BAR: ColoredStr = ColoredStr { text: "|", opt: BAR_OPT };

// help messages
const HELP_OPT: Options = Options { fg: Fg::White, bg: Bg::Default, flags: Flag::Bold };

pub(crate) static VERSION: ColoredStr = ColoredStr { text: env!("CARGO_PKG_VERSION"), opt: HELP_OPT };
pub(crate) static OPTIONS: ColoredStr = ColoredStr { text: "Options", opt: HELP_OPT };
pub(crate) static RUN_MODE: ColoredStr = ColoredStr { text: "Run mode", opt: HELP_OPT };
pub(crate) static MODE: ColoredStr = ColoredStr { text: "mode", opt: HELP_OPT };
pub(crate) static FILE: ColoredStr = ColoredStr { text: "file", opt: HELP_OPT };
pub(crate) static PATH: ColoredStr = ColoredStr { text: "path", opt: HELP_OPT };
pub(crate) static OUTPUT: ColoredStr = ColoredStr { text: "Output", opt: HELP_OPT };

pub(crate) fn info_step(step: &'static ColoredStr, path: &Path, verbosity: Verbosity) {
    if let Verbosity::Quiet = verbosity {
        return;
    }

    eprintln!("{:STEP_INDENT$}{:>STEP_PADDING$}: {}", "", step, path.display());
}

fn done(start_time: Instant, step: &'static ColoredStr, indent: usize, padding: usize) {
    let elapsed_time = Colored {
        text: format!("{}s", start_time.elapsed().as_secs_f64()),
        opt: Options { fg: Fg::White, ..Default::default() },
    };

    eprintln!("{:indent$}{:>padding$}: in {}", "", step, elapsed_time);
}

pub(crate) struct Step {
    pub(crate) start_time: Instant,
    pub(crate) verbosity: Verbosity,
}

impl Step {
    pub(crate) fn done(self) {
        if let Verbosity::Quiet = self.verbosity {
            return;
        }

        done(self.start_time, &DONE, STEP_INDENT, STEP_PADDING);
    }
}

pub(crate) struct SubStep {
    pub(crate) step: &'static ColoredStr,
    pub(crate) start_time: Instant,
    pub(crate) verbosity: Verbosity,
}

impl SubStep {
    pub(crate) fn done(self) {
        if let Verbosity::Quiet | Verbosity::Normal = self.verbosity {
            return;
        }

        done(self.start_time, self.step, SUBSTEP_INDENT, SUBSTEP_PADDING);
    }
}
