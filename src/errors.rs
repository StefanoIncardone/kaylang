use std::io::ErrorKind;
use std::{fmt::Display, borrow::Cow, path::PathBuf};

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
// TODO implement NOTE, HINT, HELP in error messages
#[derive( Debug )]
pub struct SyntaxError {
    pub col: usize,
    pub len: usize,
    pub msg: Cow<'static, str>,
    pub help_msg: Cow<'static, str>,
}

#[derive( Debug )]
pub struct SyntaxErrors {
    pub src_path: PathBuf,
    pub errors: Vec<SyntaxError>,
    pub error_idxs: Vec<(usize /* error_idx */, usize /* line_number */, usize /* line_text */)>,
    pub error_texts: Vec<String>, // TODO try making this a vec of str and inline it inside the error_idxs field
}

impl Display for SyntaxErrors {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        for (err_idx, mut line_number, line_text_idx) in &self.error_idxs {
            line_number += 1;
            let error = &self.errors[ *err_idx ];
            let line_text = &self.error_texts[ *line_text_idx ];

            let error_msg = Colored {
                text: error.msg.to_string(),
                fg: Fg::White,
                flags: Flag::Bold,
                ..Default::default()
            };

            let line_number_text = Colored {
                text: line_number.to_string(),
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
                AT, self.src_path.display(), line_number, error.col,
                BAR,
                line_number_text, BAR, line_text,
                BAR, "", pointers_and_help_msg
            )?;
        }

        return Ok( () );
    }

}

impl SyntaxErrors {
    pub(crate) fn new( errors: Vec<SyntaxError>, src: &Src ) -> Self {
        let mut this = Self {
            src_path: src.path.clone(),
            errors,
            error_idxs: Vec::new(),
            error_texts: Vec::new(),
        };

        'errors: for error in &mut this.errors {
            let mut line_number = 0;
            for line in &src.lines {
                if error.col <= line.end {
                    error.col -= line.start;
                    error.col += 1;

                    for (_, err_line_number, line_text_idx) in &this.error_idxs {
                        if line_number == *err_line_number {
                            this.error_idxs.push( (this.error_idxs.len(), line_number, *line_text_idx) );
                            continue 'errors;
                        }
                    }

                    this.error_idxs.push( (this.error_idxs.len(), line_number, this.error_texts.len() ) );
                    this.error_texts.push( src.code[ line.start..line.end ].to_string() );
                    break;
                }
                line_number += 1;
            }
        }

        this.error_idxs.sort_by_key( |err_idx| err_idx.1 );
        return this;
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
pub enum KayError {
    Src( IoError ),
    Syntax( SyntaxErrors ),
    BackEnd( IoError ),
}

impl Display for KayError {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Src( error ) | Self::BackEnd( error ) => write!( f, "{}", error ),
            Self::Syntax( errors )                      => write!( f, "{}", errors ),
        }
    }
}
