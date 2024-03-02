use super::color::{Bg, Colored, Fg, Flag, Flags};
use crate::Verbosity;
use std::{path::Path, time::Instant};

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
    pub fn info(step: &Colored<&str>, path: &Path, verbosity: Verbosity) {
        if let Verbosity::Quiet = verbosity {
            return;
        }

        eprintln!("{:STEP_INDENT$}{:>STEP_PADDING$}: {}", "", step, path.display());
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
