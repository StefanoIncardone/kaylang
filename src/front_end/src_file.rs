use crate::{error::MsgWithCause, ERROR};
use core::fmt::Display;
use std::{fs::File, io::Read as _, path::Path};
use unicode_width::UnicodeWidthChar as _;

#[expect(clippy::useless_attribute, reason = "false positive")]
#[expect(clippy::pub_use)]
pub use back_to_front::offset32;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Span {
    /// inclusive
    pub start: offset32,

    /// not inclusive
    pub end: offset32,
}

pub type Line = Span;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Position {
    pub line: offset32,
    pub column: offset32,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct DisplayPosition {
    pub line: offset32,
    pub column: offset32,
    pub display_column: offset32,
}

// IDEA(stefano): let the user chose how to obtain the source code and just store a reference to it
#[derive(Debug)]
pub struct SrcFile<'path> {
    pub(crate) path: &'path Path,
    pub(crate) code: String,
}

impl<'path> SrcFile<'path> {
    #[expect(clippy::missing_errors_doc, reason = "the code is the documentation")]
    pub fn load(path: &'path Path) -> Result<Self, Error<'path>> {
        let mut file = match File::open(path) {
            Ok(file) => file,
            Err(err) => return Err(Error { path, kind: ErrorKind::Io(err) }),
        };

        let file_metadata = match file.metadata() {
            Ok(file_metadata) => file_metadata,
            Err(err) => return Err(Error { path, kind: ErrorKind::Io(err) }),
        };

        if !file_metadata.is_file() {
            return Err(Error { path, kind: ErrorKind::MustBeAFilePath });
        }

        let file_metadata_len = file_metadata.len();
        if file_metadata_len > offset32::MAX as u64 {
            return Err(Error { path, kind: ErrorKind::FileTooBig { max: offset32::MAX } });
        };
        #[expect(clippy::cast_possible_truncation)]
        let file_len = file_metadata_len as offset32;

        let mut code = String::new();
        let bytes_read = match file.read_to_string(&mut code) {
            #[expect(clippy::cast_possible_truncation)]
            Ok(bytes_read) => bytes_read as offset32,
            Err(err) => return Err(Error { path, kind: ErrorKind::Io(err) }),
        };

        if bytes_read != file_len {
            return Err(Error { path, kind: ErrorKind::CouldNotReadEntireFile });
        }

        return Ok(Self { path, code });
    }

    #[must_use]
    #[inline(always)]
    pub const fn path(&self) -> &'path Path {
        return self.path;
    }

    #[must_use]
    #[inline(always)]
    pub fn code(&self) -> &str {
        return &self.code;
    }
}

#[derive(Debug)]
pub struct SrcCode<'code, 'path: 'code> {
    pub(crate) src_file: &'code SrcFile<'path>,
    pub(crate) lines: Vec<Line>,
}

impl<'code, 'path: 'code> SrcCode<'code, 'path> {
    #[must_use]
    #[inline(always)]
    pub const fn path(&self) -> &'path Path {
        return self.src_file.path();
    }

    #[must_use]
    #[inline(always)]
    pub fn code(&self) -> &'code str {
        return self.src_file.code();
    }

    #[must_use]
    #[inline(always)]
    pub fn lines(&self) -> &[Line] {
        return &self.lines;
    }

    #[must_use]
    fn line_index(&self, column: offset32) -> offset32 {
        let mut left: offset32 = 0;
        #[expect(clippy::cast_possible_truncation)]
        let mut right = self.lines.len() as offset32 - 1;
        while left < right {
            #[expect(clippy::integer_division, reason = "it's intended to lose precision")]
            let middle = left + (right - left) / 2;
            if column <= self.lines[middle as usize].end {
                right = middle;
            } else {
                left = middle + 1;
            }
        }
        return left;
    }

    #[must_use]
    pub(crate) fn position(&self, column: offset32) -> Position {
        let line_index = self.line_index(column);
        let line = self.lines[line_index as usize];
        let line_text_before_error = &self.code()[line.start as usize..column as usize];
        let mut utf8_column = 1;
        for _character in line_text_before_error.chars() {
            utf8_column += 1;
        }

        return Position { line: line_index + 1, column: utf8_column };
    }

    #[must_use]
    pub(crate) fn display_position(&self, column: offset32) -> DisplayPosition {
        let line_index = self.line_index(column);
        let line = self.lines[line_index as usize];
        let line_text_before_error = &self.code()[line.start as usize..column as usize];
        let mut display_column = 1;
        let mut utf8_column = 1;
        for character in line_text_before_error.chars() {
            let character_utf8_len = character.width_cjk().unwrap_or_default();
            #[expect(clippy::cast_possible_truncation)]
            {
                display_column += character_utf8_len as offset32;
            }
            utf8_column += 1;
        }

        return DisplayPosition { line: line_index + 1, column: utf8_column, display_column };
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Io(std::io::Error),
    MustBeAFilePath,
    FileTooBig { max: offset32 },
    CouldNotReadEntireFile,
}

#[derive(Debug)]
pub struct Error<'path> {
    pub path: &'path Path,
    pub kind: ErrorKind,
}

impl Display for Error<'_> {
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

#[expect(clippy::missing_trait_methods, reason = "using default implementations")]
impl core::error::Error for Error<'_> {}
