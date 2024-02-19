use crate::cli::{
    color::{Bg, Colored, Fg, Flag},
    logging::{AT, BAR, ERROR},
};
use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    path::Path,
};

use self::src_file::{Position, SrcFile};

pub mod ast;
pub mod src_file;
pub mod tokenizer;

#[derive(Debug)]
pub struct SyntaxErrorInfo {
    pub msg: Cow<'static, str>,
    pub help_msg: Cow<'static, str>,
}

pub trait SyntaxErrorKindInfo {
    fn info(&self) -> SyntaxErrorInfo;
}

#[derive(Debug)]
pub struct SyntaxError<'src, Kind: Debug + SyntaxErrorKindInfo> {
    pub path: &'src Path,
    pub position: Position,
    pub len: usize,
    pub line_text: &'src str,
    pub kind: Kind, // IDEA(stefano): split this into a msg enum and a help_msg enum
}

impl<'src, Kind: Debug + SyntaxErrorKindInfo> SyntaxError<'src, Kind> {
    pub(crate) fn new(src: &'src SrcFile, col: usize, len: usize, kind: Kind) -> Self {
        let position = src.position(col);
        let line_text = src.line_text(position);
        Self { path: &src.path, position, len, line_text, kind }
    }
}

impl<Kind: Debug + SyntaxErrorKindInfo> Display for SyntaxError<'_, Kind> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let SyntaxErrorInfo { msg, help_msg } = self.kind.info();

        let line_number = self.position.line.to_string();
        let line_number_padding = line_number.len() + 1 + BAR.text.len();

        write!(
            f,
            "{ERROR}: {error_msg}\
            \n{AT:>at_padding$}: {path}:{line}:{col}\
            \n{BAR:>line_number_padding$}\
            \n{line_number} {BAR} {line_text}\
            \n{BAR:>line_number_padding$} {spaces:>pointers_padding$}{pointers_and_help_msg}",
            error_msg = Colored { text: msg, fg: Fg::White, bg: Bg::Default, flags: Flag::Bold },
            at_padding = line_number_padding - 1,
            path = self.path.display(),
            line = self.position.line,
            col = self.position.col,
            line_number = Colored { text: line_number, fg: Fg::LightBlue, bg: Bg::Default, flags: Flag::Bold },
            line_text = self.line_text,
            spaces = "",
            pointers_padding = self.position.col - 1,
            pointers_and_help_msg = Colored {
                text: format!("{spaces:^>len$} {help_msg}", spaces = "", len = self.len),
                fg: Fg::LightRed,
                bg: Bg::Default,
                flags: Flag::Bold,
            }
        )
    }
}

impl<Kind: Debug + SyntaxErrorKindInfo> std::error::Error for SyntaxError<'_, Kind> {}
