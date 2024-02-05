use std::{time::Instant, path::Path, borrow::Cow, fmt::Display, io::{ErrorKind, IsTerminal}};

use crate::{lexer::SrcFile, Color, Position, Verbosity};

// main compilation steps (displayed when verbosity level is normal or verbose)
const STEP_OPT: Options = Options { fg: Fg::LightGreen, bg: Bg::Default, flags: Flag::Bold };
pub(crate) const STEP_PADDING: usize = 9;

pub(crate) static CHECKING:  ColoredStr = ColoredStr { text: "Checking",  opt: STEP_OPT };
pub(crate) static COMPILING: ColoredStr = ColoredStr { text: "Compiling", opt: STEP_OPT };
pub(crate) static RUNNING:   ColoredStr = ColoredStr { text: "Running",   opt: STEP_OPT };
pub(crate) static DONE:      ColoredStr = ColoredStr { text: "Done",      opt: STEP_OPT };

// sub compilation steps (displayed when verbosity lever is verbose)
const SUBSTEP_OPT: Options = Options { fg: Fg::LightBlue, bg: Bg::Default, flags: Flag::Bold };
pub(crate) const SUBSTEP_PADDING: usize = 14;

pub(crate) static LEXING:         ColoredStr = ColoredStr { text: "Lexing",         opt: SUBSTEP_OPT };
pub(crate) static AST_BUILDING:   ColoredStr = ColoredStr { text: "Ast building",   opt: SUBSTEP_OPT };
pub(crate) static ASM_GENERATION: ColoredStr = ColoredStr { text: "Asm Generation", opt: SUBSTEP_OPT };
pub(crate) static ASSEMBLER:      ColoredStr = ColoredStr { text: "Assembler",      opt: SUBSTEP_OPT };
pub(crate) static LINKER:         ColoredStr = ColoredStr { text: "Linker",         opt: SUBSTEP_OPT };
pub(crate) static SUBSTEP_DONE:   ColoredStr = ColoredStr { text: "Done",           opt: SUBSTEP_OPT };

// errors
const ERR_OPT: Options = Options { fg: Fg::LightRed, bg: Bg::Default, flags: Flag::Bold };
const BAR_OPT: Options = Options { fg: Fg::LightBlue, bg: Bg::Default, flags: Flag::Bold };

pub(crate) static ERROR: ColoredStr = ColoredStr { text: "Error", opt: ERR_OPT };
pub(crate) static CAUSE: ColoredStr = ColoredStr { text: "Cause", opt: ERR_OPT };
pub(crate) static AT:    ColoredStr = ColoredStr { text: "at",    opt: ERR_OPT };
pub(crate) static BAR:   ColoredStr = ColoredStr { text: "|",     opt: BAR_OPT };

// help messages
const HELP_OPT: Options = Options { fg: Fg::White, bg: Bg::Default, flags: Flag::Bold };

pub(crate) static VERSION:  ColoredStr = ColoredStr { text: env!( "CARGO_PKG_VERSION" ), opt: HELP_OPT };
pub(crate) static OPTIONS:  ColoredStr = ColoredStr { text: "Options",                   opt: HELP_OPT };
pub(crate) static RUN_MODE: ColoredStr = ColoredStr { text: "Run mode",                  opt: HELP_OPT };
pub(crate) static MODE:     ColoredStr = ColoredStr { text: "mode",                      opt: HELP_OPT };
pub(crate) static FILE:     ColoredStr = ColoredStr { text: "file",                      opt: HELP_OPT };
pub(crate) static PATH:     ColoredStr = ColoredStr { text: "path",                      opt: HELP_OPT };
pub(crate) static OUTPUT:   ColoredStr = ColoredStr { text: "Output",                    opt: HELP_OPT };


#[allow( non_upper_case_globals )]
static mut display: fn( &str, Options, &mut std::fmt::Formatter<'_> ) -> std::fmt::Result = Color::color;

impl Color {
    pub(crate) fn set( &self ) {
        unsafe { display = match self {
            Self::Auto   => if !std::io::stderr().is_terminal() { Self::no_color } else { Self::color },
            Self::Always => Self::color,
            Self::Never  => Self::no_color,
        } }
    }

    // since printing version and help message are the only places where printing to stdoud is
    // performed we are manually checking if stdout (overring stderr coloring modes) is in terminal
    // mode until a way to separately print colored/non-colored output to stdout/stderr is found
    pub(crate) fn set_stdout( &self ) {
        unsafe { display = match self {
            Self::Auto   => if !std::io::stdout().is_terminal() { Self::no_color } else { Self::color },
            Self::Always => Self::color,
            Self::Never  => Self::no_color,
        } }
    }


    fn no_color( text: &str, _: Options, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return text.fmt( f );
    }

    fn color( text: &str, opt: Options, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        let mut codes = String::with_capacity( 15 );

        if opt.fg != Fg::Default {
            codes += &format!( "{};", opt.fg as u8 );
        }
        if opt.bg != Bg::Default {
            codes += &format!( "{};", opt.bg as u8 );
        }
        if opt.flags & Flag::Bold != 0 {
            codes += "1;";
        }
        if opt.flags & Flag::Underline != 0 {
            codes += "4;";
        }
        if opt.flags & Flag::NoUnderline != 0 {
            codes += "24;";
        }
        if opt.flags & Flag::ReverseText != 0 {
            codes += "7;";
        }
        if opt.flags & Flag::PositiveText != 0 {
            codes += "27;";
        }

        return if codes.is_empty() {
            text.fmt( f )
        }
        else {
            codes.pop(); //remove the last ";"

            write!( f, "\x1b[{}m", codes )?;
            text.fmt( f )?;
            write!( f, "\x1b[0m" )
        }
    }
}


#[allow( dead_code )]
#[derive( Debug, Default, Clone, Copy, PartialEq )]
pub(crate) enum Fg {
    #[default] Default = 0,
    Black              = 30,
    Red                = 31,
    Green              = 32,
    Yellow             = 33,
    Blue               = 34,
    Magenta            = 35,
    Cyan               = 36,
    LightGray          = 37,
    DarkGray           = 90,
    LightRed           = 91,
    LightGreen         = 92,
    LightYellow        = 93,
    LightBlue          = 94,
    LightMagenta       = 95,
    LightCyan          = 96,
    White              = 97,
}

#[allow( dead_code )]
#[derive( Debug, Default, Clone, Copy, PartialEq )]
pub(crate) enum Bg {
    #[default] Default = 0,
    Black              = 40,
    DarkRed            = 41,
    DarkGreen          = 42,
    DarkYellow         = 43,
    DarkBlue           = 44,
    DarkMagenta        = 45,
    DarkCyan           = 46,
    DarkWhite          = 47,
    BrightBlack        = 100,
    BrightRed          = 101,
    BrightGreen        = 102,
    BrightYellow       = 103,
    BrightBlue         = 104,
    BrightMagenta      = 105,
    BrightCyan         = 106,
    White              = 107,
}

pub(crate) type Flags = u8;
pub(crate) struct Flag;

#[allow( non_upper_case_globals )]
#[allow( dead_code )]
impl Flag {
    pub(crate) const Default:      Flags = 0b0000_0000;
    pub(crate) const Bold:         Flags = 0b0000_0001;
    pub(crate) const Underline:    Flags = 0b0000_0010;
    pub(crate) const NoUnderline:  Flags = 0b0000_0100;
    pub(crate) const ReverseText:  Flags = 0b0000_1000;
    pub(crate) const PositiveText: Flags = 0b0001_0000;
}


#[derive( Debug, Default, Clone, Copy )]
pub(crate) struct Options {
    pub(crate) fg: Fg,
    pub(crate) bg: Bg,
    pub(crate) flags: Flags,
}

#[derive( Debug, Default )]
pub(crate) struct ColoredStr {
    pub(crate) text: &'static str,
    pub(crate) opt: Options,
}

impl Display for ColoredStr {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return unsafe { display( self.text, self.opt, f ) };
    }
}

#[derive( Debug, Default )]
pub(crate) struct Colored {
    pub(crate) text: String,
    pub(crate) opt: Options,
}

impl Display for Colored {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return unsafe { display( &self.text, self.opt, f ) };
    }
}


// TODO(stefano): move compilation steps measurements to individual parts (lexing, ast building, ...)
pub(crate) struct CompilationLogger {
    start_time: Instant,
    step_time: Instant,
    substep_time: Instant,
    verbosity: Verbosity,
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
            opt: Options { fg: Fg::White, ..Default::default() },
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
            Verbosity::Quiet => {},
            Verbosity::Normal | Verbosity::Verbose => self.substep_display( &self.start_time, 0, &DONE, STEP_PADDING ),
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

// TODO(stefano): move errors to different file
// TODO(stefano): create specilized instanced of specific errors
#[derive( Debug )]
pub struct CliError {
    pub msg: Cow<'static, str>,
}

impl Display for CliError {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "{}: {}", ERROR, self.msg );
    }
}


#[derive( Debug )]
pub(crate) struct RawSyntaxError {
    pub(crate) col: usize,
    pub(crate) len: usize,
    pub(crate) msg: Cow<'static, str>,
    pub(crate) help_msg: Cow<'static, str>,
}

// TODO(stefano): implement NOTE, HINT, HELP in error messages
#[derive( Debug )]
pub struct SyntaxError {
    pub line: usize,
    pub col: usize,
    pub len: usize,
    pub msg: Cow<'static, str>,
    pub help_msg: Cow<'static, str>,
}

#[derive( Debug )]
pub struct SyntaxErrors<'src> {
    pub(crate) src: &'src SrcFile,
    pub errors: Vec<SyntaxError>,
}

impl Display for SyntaxErrors<'_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        for error in &self.errors {
            let line = &self.src.lines[ error.line - 1 ];
            let line_text = &self.src.code[ line.start..line.end ];

            let error_msg = Colored {
                text: error.msg.to_string(),
                opt: Options { fg: Fg::White, bg: Bg::Default, flags: Flag::Bold }
            };

            let line_number_text = Colored {
                text: error.line.to_string(),
                opt: Options { fg: Fg::LightBlue, bg: Bg::Default, flags: Flag::Bold }
            };

            let visualization_padding = line_number_text.text.len() + 1 + BAR.text.len();
            let at_padding = visualization_padding - 1;

            let pointers_col = error.col - 1;
            let pointers_len = error.len;

            let pointers_and_help_msg = Colored {
                text: format!( "{:>pointers_col$}{:^>pointers_len$} {}", "", "", error.help_msg ),
                opt: Options { fg: Fg::LightRed, bg: Bg::Default, flags: Flag::Bold }
            };

            writeln!( f,
                "{}: {}\
                \n{:>at_padding$}: {}:{}:{}\
                \n{:>visualization_padding$}\
                \n{} {} {}\
                \n{:>visualization_padding$} {}\n",
                ERROR, error_msg,
                AT, self.src.path.display(), error.line, error.col,
                BAR,
                line_number_text, BAR, line_text,
                BAR, pointers_and_help_msg
            )?;
        }

        return Ok( () );
    }
}

pub(crate) trait AddError<'src> {
    fn add( &mut self, src: &'src SrcFile, error: RawSyntaxError );
}

impl<'src> AddError<'src> for Vec<SyntaxError> {
    fn add( &mut self, src: &'src SrcFile, error: RawSyntaxError ) {
        let Position { line, col } = src.position( error.col );
        self.push( SyntaxError {
            line,
            col,
            len: error.len,
            msg: error.msg.clone(),
            help_msg: error.help_msg.clone(),
        } );
    }
}


#[derive( Debug )]
pub struct IoError {
    pub kind: ErrorKind,
    pub msg: Cow<'static, str>,
    pub cause: Cow<'static, str>,
}

impl Display for IoError {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f,
            "{}: {} [{}]
            \n{}: {}",
            ERROR, self.msg, self.kind,
            CAUSE, self.cause
        );
    }
}


#[derive( Debug )]
pub enum KayError<'src> {
    Src( IoError ),
    Syntax( SyntaxErrors<'src> ),
    Compilation( IoError ),
    Running( IoError ),
}

impl Display for KayError<'_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Src( err ) | Self::Compilation( err ) | Self::Running( err ) => write!( f, "{}", err ),
            Self::Syntax( err )                                                => write!( f, "{}", err ),
        }
    }
}
