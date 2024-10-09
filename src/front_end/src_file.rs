use crate::{error::MsgWithCause, ERROR};
use core::fmt::Display;
use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};
use unicode_width::UnicodeWidthChar;

#[allow(non_camel_case_types)]
pub type offset32 = u32;
#[allow(non_camel_case_types)]
pub type line32 = u32;
#[allow(non_camel_case_types)]
pub type column32 = u32;
#[allow(non_camel_case_types)]
pub type index32 = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// inclusive
    pub start: offset32,

    /// not inclusive
    pub end: offset32,
}

pub type Line = Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: line32,
    pub column: column32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DisplayPosition {
    pub line: line32,
    pub column: column32,
    pub display_column: column32,
}

#[derive(Debug)]
pub struct SrcFile {
    pub(crate) path: PathBuf,
    pub(crate) code: String,
    pub(crate) lines: Vec<Line>,
}

impl SrcFile {
    // IDEA(stefano): move parsing of lines for bounds into the tokenizer
    pub fn load(path: &Path) -> Result<Self, Error> {
        let path_buf = path.to_owned();
        let mut file = match File::open(path) {
            Ok(file) => file,
            Err(err) => return Err(Error { path: path_buf, kind: ErrorKind::Io(err) }),
        };

        let file_len = match file.metadata() {
            Ok(metadata) => {
                if !metadata.is_file() {
                    return Err(Error { path: path_buf, kind: ErrorKind::MustBeAFilePath });
                }

                let file_len = metadata.len();
                if file_len > column32::MAX as u64 {
                    return Err(Error {
                        path: path_buf,
                        kind: ErrorKind::FileTooBig { max: column32::MAX },
                    });
                }
                file_len as column32
            }
            Err(err) => return Err(Error { path: path_buf, kind: ErrorKind::Io(err) }),
        };

        let code = {
            let mut code = String::with_capacity(file_len as usize);
            let bytes_read = match file.read_to_string(&mut code) {
                Ok(bytes_read) => bytes_read as column32,
                Err(err) => return Err(Error { path: path_buf, kind: ErrorKind::Io(err) }),
            };

            if bytes_read != file_len {
                return Err(Error { path: path_buf, kind: ErrorKind::CouldNotReadEntireFile });
            }

            code
        };

        let mut lines = Vec::<Line>::new();
        let mut start = 0;
        let mut current_ascii_column: column32 = 0;

        let code_ascii = code.as_bytes();
        let code_bytes_len = code_ascii.len() as column32;
        while current_ascii_column < code_bytes_len {
            match code_ascii[current_ascii_column as usize] {
                b'\n' => {
                    // we reached the end of the line on a LF (\n)
                    lines.push(Line { start, end: current_ascii_column });
                    start = current_ascii_column + 1;
                }
                b'\r' => {
                    let Some(possible_new_line) = code_ascii.get(current_ascii_column as usize + 1)
                    else {
                        // we reached the end of the file on a stray \r
                        lines.push(Line { start, end: current_ascii_column });
                        break;
                    };

                    if *possible_new_line == b'\n' {
                        // we reached the end of the line on a CRLF (\r\n)
                        lines.push(Line { start, end: current_ascii_column });
                        current_ascii_column += 1;
                        start = current_ascii_column + 1;
                    }
                }
                _ => {}
            }

            current_ascii_column += 1;
        }

        if !code.is_empty() && code_ascii[current_ascii_column as usize - 1] != b'\n' {
            // we reached the end of the file on a line without a trailing \n
            lines.push(Line { start, end: current_ascii_column });
        }

        return Ok(Self { path: path_buf, code, lines });
    }

    #[must_use]
    #[inline(always)]
    pub fn path(&self) -> &Path {
        return &self.path;
    }

    #[must_use]
    #[inline(always)]
    pub fn code(&self) -> &str {
        return &self.code;
    }

    #[must_use]
    #[inline(always)]
    pub fn lines(&self) -> &[Line] {
        return &self.lines;
    }

    #[must_use]
    pub fn position(&self, column: offset32) -> Position {
        let mut left: index32 = 0;
        let mut right = self.lines.len() as index32 - 1;
        while left < right {
            #[allow(clippy::integer_division)] // it's intended to lose precision
            let middle = left + (right - left) / 2;
            if column < self.lines[middle as usize].end {
                right = middle;
            } else {
                left = middle + 1;
            }
        }

        let line = &self.lines[left as usize];
        let line_text_before_error = &self.code[line.start as usize..column as usize];
        let mut utf8_column = 1;
        for _character in line_text_before_error.chars() {
            utf8_column += 1;
        }

        return Position { line: left + 1, column: utf8_column };
    }

    #[must_use]
    pub fn display_position(&self, column: offset32) -> DisplayPosition {
        let mut left: index32 = 0;
        let mut right = self.lines.len() as index32 - 1;
        while left < right {
            #[allow(clippy::integer_division)] // it's intended to lose precision
            let middle = left + (right - left) / 2;
            if column < self.lines[middle as usize].end {
                right = middle;
            } else {
                left = middle + 1;
            }
        }

        let line = &self.lines[left as usize];
        let line_text_before_error = &self.code[line.start as usize..column as usize];
        let mut display_column = 1;
        let mut utf8_column = 1;
        for character in line_text_before_error.chars() {
            let character_utf8_len = match character.width_cjk() {
                Some(character_utf8_len) => character_utf8_len,
                None => 1,
            };
            display_column += character_utf8_len as column32;
            utf8_column += 1;
        }

        return DisplayPosition { line: left + 1, column: utf8_column, display_column };
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Io(std::io::Error),
    MustBeAFilePath,
    FileTooBig { max: column32 },
    CouldNotReadEntireFile,
}

#[derive(Debug)]
pub struct Error {
    pub path: PathBuf,
    pub kind: ErrorKind,
}

impl Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let message = format!("could not read '{}'", self.path.display());
        let cause = match &self.kind {
            ErrorKind::Io(err) => format!("{err} ({})", err.kind()),
            ErrorKind::MustBeAFilePath => "must be a file path".to_owned(),
            ErrorKind::FileTooBig { max } => {
                format!("file exceeds the size limit of {max} bytes")
            }
            ErrorKind::CouldNotReadEntireFile => "failed to read entire file".to_owned(),
        };

        let error = MsgWithCause { kind: &ERROR, message: &message, cause: &cause };
        return write!(f, "{error}");
    }
}

impl std::error::Error for Error {}
