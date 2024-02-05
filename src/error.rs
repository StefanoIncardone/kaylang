use std::{
    borrow::Cow,
    fmt::Display,
    io::{self, ErrorKind},
    path::PathBuf,
};

use crate::{Bg, Colored, Fg, Flag, Options, Position, SrcFile, AT, BAR, CAUSE, ERROR};

#[derive(Debug)]
pub enum SrcParseError {
    CouldNotOpen(CouldNotOpen),
    ExpectedFile(ExpectedFile),
    CouldNotReadMetadata(CouldNotReadMetadata),
    CouldNotReadContents(CouldNotReadContents),
}

impl Display for SrcParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::CouldNotOpen(err) => write!(f, "{}", err),
            Self::ExpectedFile(err) => write!(f, "{}", err),
            Self::CouldNotReadMetadata(err) => write!(f, "{}", err),
            Self::CouldNotReadContents(err) => write!(f, "{}", err),
        };
    }
}

impl std::error::Error for SrcParseError {}

#[derive(Debug)]
pub struct CouldNotOpen {
    pub err: io::Error,
    pub path: PathBuf,
}

impl Display for CouldNotOpen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            "{}: could not open '{}'\
            \n{}: {} ({})",
            ERROR,
            self.path.display(),
            CAUSE,
            self.err,
            self.err.kind(),
        );
    }
}

impl std::error::Error for CouldNotOpen {}

#[derive(Debug)]
pub struct ExpectedFile {
    pub path: PathBuf,
}

impl Display for ExpectedFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            "{}: invalid path '{}'\
            \n{}: expected a file but got a directory",
            ERROR,
            self.path.display(),
            CAUSE,
        );
    }
}

impl std::error::Error for ExpectedFile {}

#[derive(Debug)]
pub struct CouldNotReadMetadata {
    pub err: io::Error,
    pub path: PathBuf,
}

impl Display for CouldNotReadMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            "{}: could not read metadata of '{}'\
            \n{}: {} ({})",
            ERROR,
            self.path.display(),
            CAUSE,
            self.err,
            self.err.kind(),
        );
    }
}

impl std::error::Error for CouldNotReadMetadata {}

#[derive(Debug)]
pub struct CouldNotReadContents {
    pub err: io::Error,
    pub path: PathBuf,
}

impl Display for CouldNotReadContents {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            "{}: could not read contents of '{}'\
            \n{}: {} ({})",
            ERROR,
            self.path.display(),
            CAUSE,
            self.err,
            self.err.kind(),
        );
    }
}

impl std::error::Error for CouldNotReadContents {}

// TODO(stefano): move errors to different file
// TODO(stefano): create specilized instanced of specific errors
#[derive(Debug)]
pub struct CliError {
    pub msg: Cow<'static, str>,
}

impl Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{}: {}", ERROR, self.msg);
    }
}

#[derive(Debug)]
pub(crate) struct RawSyntaxError {
    pub(crate) col: usize,
    pub(crate) len: usize,
    pub(crate) msg: Cow<'static, str>,
    pub(crate) help_msg: Cow<'static, str>,
}

// TODO(stefano): implement NOTE, HINT, HELP in error messages
#[derive(Debug)]
pub struct SyntaxError {
    pub line: usize,
    pub col: usize,
    pub len: usize,
    pub msg: Cow<'static, str>,
    pub help_msg: Cow<'static, str>,
}

#[derive(Debug)]
pub struct SyntaxErrors<'src> {
    pub(crate) src: &'src SrcFile,
    pub errors: Vec<SyntaxError>,
}

impl Display for SyntaxErrors<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for error in &self.errors {
            let line = &self.src.lines[error.line - 1];
            let line_text = &self.src.code[line.start..line.end];

            let error_msg = Colored {
                text: error.msg.to_string(),
                opt: Options { fg: Fg::White, bg: Bg::Default, flags: Flag::Bold },
            };

            let line_number_text = Colored {
                text: error.line.to_string(),
                opt: Options { fg: Fg::LightBlue, bg: Bg::Default, flags: Flag::Bold },
            };

            let visualization_padding = line_number_text.text.len() + 1 + BAR.text.len();
            let at_padding = visualization_padding - 1;

            let pointers_col = error.col - 1;
            let pointers_len = error.len;

            let pointers_and_help_msg = Colored {
                text: format!("{:>pointers_col$}{:^>pointers_len$} {}", "", "", error.help_msg),
                opt: Options { fg: Fg::LightRed, bg: Bg::Default, flags: Flag::Bold },
            };

            writeln!(
                f,
                "{}: {}\
                \n{:>at_padding$}: {}:{}:{}\
                \n{:>visualization_padding$}\
                \n{} {} {}\
                \n{:>visualization_padding$} {}\n",
                ERROR,
                error_msg,
                AT,
                self.src.path.display(),
                error.line,
                error.col,
                BAR,
                line_number_text,
                BAR,
                line_text,
                BAR,
                pointers_and_help_msg
            )?;
        }

        return Ok(());
    }
}

pub(crate) trait AddError<'src> {
    fn add(&mut self, src: &'src SrcFile, error: RawSyntaxError);
}

impl<'src> AddError<'src> for Vec<SyntaxError> {
    fn add(&mut self, src: &'src SrcFile, error: RawSyntaxError) {
        let Position { line, col } = src.position(error.col);
        self.push(SyntaxError { line, col, len: error.len, msg: error.msg.clone(), help_msg: error.help_msg.clone() });
    }
}

#[derive(Debug)]
pub struct IoError {
    pub kind: ErrorKind,
    pub msg: Cow<'static, str>,
    pub cause: Cow<'static, str>,
}

impl Display for IoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            "{}: {} [{}]
            \n{}: {}",
            ERROR, self.msg, self.kind, CAUSE, self.cause
        );
    }
}

#[derive(Debug)]
pub enum KayError<'src> {
    Src(IoError),
    Syntax(SyntaxErrors<'src>),
    Compilation(IoError),
    Running(IoError),
}

impl Display for KayError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Src(err) | Self::Compilation(err) | Self::Running(err) => write!(f, "{}", err),
            Self::Syntax(err) => write!(f, "{}", err),
        };
    }
}
