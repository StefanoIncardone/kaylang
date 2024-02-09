use std::{fmt::Display, io::IsTerminal, path::Path, time::Instant};

use crate::{
    color::{Bg, Colored, ColoredStr, Fg, Flag},
    Color, Flags, Verbosity,
};

// main compilation steps (displayed when verbosity level is normal or verbose)
const STEP_FG: Fg = Fg::LightGreen;
const STEP_BG: Bg = Bg::Default;
const STEP_FLAGS: Flags = Flag::Bold;
const STEP_INDENT: usize = 0;
const STEP_PADDING: usize = 9;

#[rustfmt::skip] pub static CHECKING:  ColoredStr = ColoredStr { text: "Checking",  fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static COMPILING: ColoredStr = ColoredStr { text: "Compiling", fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static RUNNING:   ColoredStr = ColoredStr { text: "Running",   fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static DONE:      ColoredStr = ColoredStr { text: "Done",      fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };

// sub compilation steps (displayed when verbosity lever is verbose)
const SUBSTEP_FG: Fg = Fg::LightBlue;
const SUBSTEP_BG: Bg = Bg::Default;
const SUBSTEP_FLAGS: Flags = Flag::Bold;
const SUBSTEP_INDENT: usize = 4;
const SUBSTEP_PADDING: usize = 14;

#[rustfmt::skip] pub static LOADING_SOURCE: ColoredStr = ColoredStr { text: "Loding Source",  fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static LEXING:         ColoredStr = ColoredStr { text: "Lexing",         fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static AST_BUILDING:   ColoredStr = ColoredStr { text: "Ast building",   fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static ASM_GENERATION: ColoredStr = ColoredStr { text: "Asm Generation", fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static ASSEMBLER:      ColoredStr = ColoredStr { text: "Assembler",      fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static LINKER:         ColoredStr = ColoredStr { text: "Linker",         fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static SUBSTEP_DONE:   ColoredStr = ColoredStr { text: "Done",           fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };

// errors
const ERR_FG: Fg = Fg::LightRed;
const ERR_BG: Bg = Bg::Default;
const ERR_FLAGS: Flags = Flag::Bold;

const BAR_FG: Fg = Fg::LightBlue;
const BAR_BG: Bg = Bg::Default;
const BAR_FLAGS: Flags = Flag::Bold;

#[rustfmt::skip] pub(crate) static ERROR: ColoredStr = ColoredStr { text: "Error", fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static CAUSE: ColoredStr = ColoredStr { text: "Cause", fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static AT:    ColoredStr = ColoredStr { text: "at",    fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static BAR:   ColoredStr = ColoredStr { text: "|",     fg: BAR_FG, bg: BAR_BG, flags: BAR_FLAGS };

// help messages
const HELP_FG: Fg = Fg::White;
const HELP_BG: Bg = Bg::Default;
const HELP_FLAGS: Flags = Flag::Bold;

#[rustfmt::skip] pub(crate) static VERSION:  ColoredStr = ColoredStr { text: env!("CARGO_PKG_VERSION"), fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static OPTIONS:  ColoredStr = ColoredStr { text: "Options",                 fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static RUN_MODE: ColoredStr = ColoredStr { text: "Run mode",                fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static MODE:     ColoredStr = ColoredStr { text: "mode",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static FILE:     ColoredStr = ColoredStr { text: "file",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static PATH:     ColoredStr = ColoredStr { text: "path",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static OUTPUT:   ColoredStr = ColoredStr { text: "Output",                  fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };

fn done(start_time: Instant, step: &ColoredStr, indent: usize, padding: usize) {
    let elapsed_time =
        Colored { text: format!("{}s", start_time.elapsed().as_secs_f32()), fg: Fg::White, ..Default::default() };

    eprintln!("{:indent$}{:>padding$}: in {}", "", step, elapsed_time);
}

pub struct Step {
    pub start_time: Instant, // TODO(stefano): make private eventually
    pub verbosity: Verbosity,
}

impl Step {
    pub fn info(step: &ColoredStr, path: &Path, verbosity: Verbosity) {
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

pub struct SubStep {
    pub step: &'static ColoredStr,
    pub start_time: Instant, // TODO(stefano): make private eventually
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

#[allow(non_upper_case_globals)]
pub(crate) static mut log: fn(&str, Fg, Bg, Flags, &mut std::fmt::Formatter<'_>) -> std::fmt::Result = log_color;

impl Color {
    pub fn set(self, sink: impl IsTerminal) {
        unsafe {
            log = match self {
                Color::Auto => {
                    if sink.is_terminal() {
                        log_color
                    } else {
                        log_no_color
                    }
                }
                Color::Always => log_color,
                Color::Never => log_no_color,
            }
        }
    }
}

fn log_no_color(text: &str, _: Fg, _: Bg, _: Flags, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return text.fmt(f);
}

fn log_color(text: &str, fg: Fg, bg: Bg, flags: Flags, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut codes = String::with_capacity(15);

    if fg != Fg::Default {
        codes += &format!("{};", fg as u8);
    }
    if bg != Bg::Default {
        codes += &format!("{};", bg as u8);
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

        write!(f, "\x1b[{}m", codes)?;
        text.fmt(f)?;
        write!(f, "\x1b[0m")
    };
}
