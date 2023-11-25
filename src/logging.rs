use std::{time::Instant, path::Path};

use crate::{color::*, Verbosity};

// main compilation steps (displayed when verbosity level is normal or verbose)
const STEP_FG:      Fg    = Fg::LightGreen;
const STEP_BG:      Bg    = Bg::Default;
const STEP_FLAGS:   Flags = Flag::Bold;
pub(crate) const STEP_PADDING: usize = 9;

pub(crate) static CHECKING:  ColoredStr = ColoredStr { text: "Checking",  fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
pub(crate) static COMPILING: ColoredStr = ColoredStr { text: "Compiling", fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
pub(crate) static RUNNING:   ColoredStr = ColoredStr { text: "Running",   fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
pub(crate) static DONE:      ColoredStr = ColoredStr { text: "Done",      fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };

// sub compilation steps (displayed when verbosity lever is verbose)
const SUBSTEP_FG:      Fg    = Fg::LightBlue;
const SUBSTEP_BG:      Bg    = Bg::Default;
const SUBSTEP_FLAGS:   Flags = Flag::Bold;
pub(crate) const SUBSTEP_PADDING: usize = 14;

pub(crate) static LEXING:         ColoredStr = ColoredStr { text: "Lexing",         fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
pub(crate) static PARSING:        ColoredStr = ColoredStr { text: "Parsing",        fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
pub(crate) static ASM_GENERATION: ColoredStr = ColoredStr { text: "Asm Generation", fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
pub(crate) static ASSEMBLER:      ColoredStr = ColoredStr { text: "Assembler",      fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
pub(crate) static LINKER:         ColoredStr = ColoredStr { text: "Linker",         fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
pub(crate) static SUBSTEP_DONE:   ColoredStr = ColoredStr { text: "Done",           fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };

// errors
const ERR_FG:    Fg    = Fg::LightRed;
const ERR_BG:    Bg    = Bg::Default;
const ERR_FLAGS: Flags = Flag::Bold;

pub(crate) static ERROR: ColoredStr = ColoredStr { text: "Error", fg: ERR_FG,        bg: ERR_BG, flags: ERR_FLAGS };
pub(crate) static CAUSE: ColoredStr = ColoredStr { text: "Cause", fg: ERR_FG,        bg: ERR_BG, flags: ERR_FLAGS };
pub(crate) static AT:    ColoredStr = ColoredStr { text: "at",    fg: ERR_FG,        bg: ERR_BG, flags: ERR_FLAGS };
pub(crate) static BAR:   ColoredStr = ColoredStr { text: "|",     fg: Fg::LightBlue, bg: ERR_BG, flags: ERR_FLAGS };

// help messages
const HELP_FG:    Fg    = Fg::White;
const HELP_BG:    Bg    = Bg::Default;
const HELP_FLAGS: Flags = Flag::Bold;

pub(crate) static VERSION:  ColoredStr = ColoredStr { text: env!( "CARGO_PKG_VERSION" ), fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
pub(crate) static OPTIONS:  ColoredStr = ColoredStr { text: "Options",                   fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
pub(crate) static RUN_MODE: ColoredStr = ColoredStr { text: "Run mode",                  fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
pub(crate) static MODE:     ColoredStr = ColoredStr { text: "mode",                      fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
pub(crate) static FILE:     ColoredStr = ColoredStr { text: "file",                      fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
pub(crate) static PATH:     ColoredStr = ColoredStr { text: "path",                      fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
pub(crate) static OUTPUT:   ColoredStr = ColoredStr { text: "Output",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };


pub struct CompilationLogger {
    start_time: Instant,
    step_time: Instant,
    substep_time: Instant,
    pub(crate) verbosity: Verbosity,
}

impl CompilationLogger {
    pub(crate) fn new( verbosity: Verbosity ) -> Self {
        let now = Instant::now();
        return Self {
            start_time: now,
            step_time: now,
            substep_time: now,
            verbosity,
        };
    }


    pub(crate) fn step_display( &self, step: &'static ColoredStr, path: &Path ) {
        eprintln!( "{:>STEP_PADDING$}: {}", step, path.display() );
    }

    pub(crate) fn substep_display( &self, start_time: &Instant, indent: usize, step: &'static ColoredStr, padding: usize ) {
        let elapsed_time = Colored {
            text: format!( "{}s", start_time.elapsed().as_secs_f64() ),
            fg: Fg::White,
            ..Default::default()
        };

        eprintln!( "{:indent$}{:>padding$}: in {}", "", step, elapsed_time );
    }


    pub(crate) fn step( &mut self, step: &'static ColoredStr, path: &Path ) {
        match self.verbosity {
            Verbosity::Quiet => {},
            Verbosity::Normal | Verbosity::Verbose => self.step_display( step, path ),
        }
    }

    pub(crate) fn done( &mut self ) {
        match self.verbosity {
            Verbosity::Quiet | Verbosity::Normal => {},
            Verbosity::Verbose => self.substep_display( &self.start_time, 0, &DONE, STEP_PADDING ),
        }
    }

    pub(crate) fn substep( &mut self, step: &'static ColoredStr ) {
        match self.verbosity {
            Verbosity::Quiet | Verbosity::Normal => {},
            Verbosity::Verbose => {
                self.substep_display( &self.substep_time, 4, step, SUBSTEP_PADDING );
                self.substep_time = Instant::now();
            },
        }
    }

    pub(crate) fn substep_done( &mut self ) {
        match self.verbosity {
            Verbosity::Quiet | Verbosity::Normal => {},
            Verbosity::Verbose => {
                self.substep_display( &self.step_time, 4, &SUBSTEP_DONE, SUBSTEP_PADDING );
                let new_step_time = Instant::now();
                (self.step_time, self.substep_time) = (new_step_time, new_step_time);
            },
        }
    }
}
