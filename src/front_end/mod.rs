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
    msg: Cow<'static, str>,
    help_msg: Cow<'static, str>,
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
    pub kind: Kind,
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

        let error_msg = Colored { text: msg.to_string(), fg: Fg::White, bg: Bg::Default, flags: Flag::Bold };

        let line_number_text =
            Colored { text: self.position.line.to_string(), fg: Fg::LightBlue, bg: Bg::Default, flags: Flag::Bold };

        let visualization_padding = line_number_text.text.len() + 1 + BAR.text.len();
        let at_padding = visualization_padding - 1;

        let pointers_col = self.position.col - 1;
        let pointers_len = self.len;

        let pointers_and_help_msg = Colored {
            text: format!("{:>pointers_col$}{:^>pointers_len$} {help_msg}", "", ""),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        write!(
            f,
            "{ERROR}: {error_msg}\
            \n{AT:>at_padding$}: {path}:{line}:{col}\
            \n{BAR:>visualization_padding$}\
            \n{line_number_text} {BAR} {line_text}\
            \n{BAR:>visualization_padding$} {pointers_and_help_msg}",
            path = self.path.display(),
            line = self.position.line,
            col = self.position.col,
            line_text = self.line_text,
        )
    }
}

impl<Kind: Debug + SyntaxErrorKindInfo> std::error::Error for SyntaxError<'_, Kind> {}
