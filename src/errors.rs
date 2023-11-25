use std::io::ErrorKind;
use std::{fmt::Display, borrow::Cow};

use crate::{Src, logging::*, color::*};


#[derive( Debug )]
pub struct CliError {
    pub msg: Cow<'static, str>,
}

impl Display for CliError {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return write!( f, "{}: {}", ERROR, self.msg );
    }
}


// TODO implement SyntaxErrorsDisplay struct that gets constructed on demand when needing to print errors
#[derive( Debug )]
pub struct SyntaxError<'src> {
    pub line: usize,
    pub col: usize,
    pub len: usize,
    pub line_text: &'src str,
    pub msg: Cow<'static, str>,
    pub help_msg: Cow<'static, str>,
}

#[derive( Debug )]
pub struct SyntaxErrors<'src> {
    pub src: &'src Src,
    pub errors: Vec<SyntaxError<'src>>,
}

impl Display for SyntaxErrors<'_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        for error in &self.errors {
            let error_msg = Colored {
                text: error.msg.to_string(),
                fg: Fg::White,
                flags: Flag::Bold,
                ..Default::default()
            };

            let line_number_text = Colored {
                text: error.line.to_string(),
                fg: Fg::LightBlue,
                flags: Flag::Bold,
                ..Default::default()
            };

            let visualization_padding = line_number_text.text.len() + 1 + BAR.text.len();
            let at_padding = visualization_padding - 1;

            let pointers_col = error.col - 1;
            let pointers_len = error.len;

            let pointers_and_help_msg = Colored {
                text: format!( "{:^>pointers_len$} {}", "", error.help_msg ),
                fg: Fg::LightRed,
                flags: Flag::Bold,
                ..Default::default()
            };

            writeln!( f,
                "{}: {}\
                \n{:>#at_padding$}: {}:{}:{}\
                \n{:>#visualization_padding$}\
                \n{} {} {}\
                \n{:>#visualization_padding$} {:>pointers_col$}{}\n",
                ERROR, error_msg,
                AT, self.src.path.display(), error.line, error.col,
                BAR,
                line_number_text, BAR, error.line_text,
                BAR, "", pointers_and_help_msg
            )?;
        }

        return Ok( () );
    }
}

impl<'src> From<RawSyntaxErrors<'src>> for SyntaxErrors<'src> {
    fn from( errors: RawSyntaxErrors<'src> ) -> Self {
        let mut this = SyntaxErrors {
            src: errors.src,
            errors: Vec::new(),
        };

        for error in &errors.errors {
            for (line_number, line) in errors.src.lines.iter().enumerate() {
                if error.col <= line.end {
                    this.errors.push( SyntaxError {
                        line: line_number + 1,
                        col: error.col + 1 - line.start,
                        len: error.len,
                        line_text: &errors.src.code[ line.start..line.end],
                        msg: error.msg.clone(),
                        help_msg: error.help_msg.clone(),
                    } );
                    break;
                }
            }
        }

        return this;
    }
}

// TODO implement NOTE, HINT, HELP in error messages
#[derive( Debug )]
pub(crate) struct RawSyntaxError {
    pub(crate) col: usize,
    pub(crate) len: usize,
    pub(crate) msg: Cow<'static, str>,
    pub(crate) help_msg: Cow<'static, str>,
}

#[derive( Debug )]
pub(crate) struct RawSyntaxErrors<'src> {
    pub(crate) src: &'src Src,
    pub(crate) errors: Vec<RawSyntaxError>,
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
pub enum CheckError<'src> {
    Src( IoError ),
    Syntax( SyntaxErrors<'src> ),
}

impl Display for CheckError<'_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Src( error ) => write!( f, "{}", error ),
            Self::Syntax( errors )                      => write!( f, "{}", errors ),
        }
    }
}

#[derive( Debug )]
pub enum CompileError<'src> {
    Check( CheckError<'src> ),
    BackEnd( IoError ),
}

impl Display for CompileError<'_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Check( error ) => write!( f, "{}", error ),
            Self::BackEnd( errors )                      => write!( f, "{}", errors ),
        }
    }
}

#[derive( Debug )]
pub enum KayError<'src> {
    Src( IoError ),
    Syntax( SyntaxErrors<'src> ),
    BackEnd( IoError ),
}

impl Display for KayError<'_> {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Src( err ) | Self::BackEnd( err ) => write!( f, "{}", err ),
            Self::Syntax( err )                     => write!( f, "{}", err ),
        }
    }
}
