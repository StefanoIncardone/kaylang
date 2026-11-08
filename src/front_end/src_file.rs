use crate::{ERROR, error::MsgWithCause, front_end::{Index32, MsgSeverity}};
use std::{fs::File, io::Read as _, path::Path};
use back_to_front::{slice, src_code::Span, text};
use unicode_width::UnicodeWidthChar as _;

#[expect(clippy::useless_attribute, reason = "false positive")]
#[expect(clippy::pub_use)]
pub use back_to_front::usize32;


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct Position<'path> {
    pub file: &'path Path,
    /// starts from 1
    pub line: usize32,
    /// starts from 1, relative to the line, aware of utf8 characters
    pub column: usize32,
    /// starts from 0, relative to the start of the file
    pub offset: usize32,
    /// graphemes display width
    pub width: usize32,
}

impl<'path> Position<'path> {
    pub(crate) fn new(src: &SrcCode<'_, 'path>, offset: usize32) -> Self {
        let line_index = src.line_index(offset);
        let line_span = src.line_span(line_index);
        let line_text_before_error = text::slice(src.code(), line_span.start() as usize, offset as usize);

        let mut width = 1;
        let mut column = 1;
        for character in line_text_before_error.chars() {
            let character_width = character.width_cjk().unwrap_or_default();
            width += character_width as usize32;
            column += 1;
        }

        return Self { file: src.path(), line: line_index.0 + 1, column, offset, width };
    }
}

impl core::fmt::Display for Position<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let Self { file: file_raw, line, column, offset, .. } = self;
        let file = file_raw.display();
        return write!(f, "{file}:{line}:{column}:{offset}");
    }
}

// IDEA(stefano): let the user chose how to obtain the source code and just store a reference to it
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[must_use]
pub struct SrcFile<'path> {
    pub(crate) path: &'path Path,
    pub(crate) code: String,
}

// getters
#[rustfmt::skip]
impl<'path> SrcFile<'path> {
    #[must_use] #[inline(always)] pub const fn path(&self) -> &'path Path { return self.path; }
    #[must_use] #[inline(always)] pub const fn code(&self) -> &str        { return self.code.as_str(); }

    #[must_use] #[inline(always)] #[track_caller]
    pub const fn text(&self, start: usize32, end: usize32) -> &str {
        return text::slice(self.code(), start as usize, end as usize);
    }
}

impl<'path> SrcFile<'path> {
    #[expect(clippy::missing_errors_doc)]
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
        if file_metadata_len > usize32::MAX as u64 {
            return Err(Error { path, kind: ErrorKind::FileTooBig { max: usize32::MAX } });
        }
        let file_len = file_metadata_len as usize32;

        let mut code = String::new();
        let bytes_read = match file.read_to_string(&mut code) {
            Ok(bytes_read) => bytes_read as usize32,
            Err(err) => return Err(Error { path, kind: ErrorKind::Io(err) }),
        };

        if bytes_read != file_len {
            return Err(Error { path, kind: ErrorKind::CouldNotReadEntireFile });
        }

        return Ok(Self { path, code });
    }
}

/// Consecutive ranges of line end columns, excluding newline characters
pub type LineEnds = Vec<usize32>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SrcCode<'code, 'path> {
    pub(crate) src_file: &'code SrcFile<'path>,
    pub(crate) line_ends: LineEnds,
}

// getters
#[rustfmt::skip]
impl<'code, 'path> SrcCode<'code, 'path> {
    #[must_use] #[inline(always)] pub const fn path(&self)      -> &'path Path { return self.src_file.path(); }
    #[must_use] #[inline(always)] pub const fn code(&self)      -> &'code str  { return self.src_file.code(); }
    #[must_use] #[inline(always)] pub const fn line_ends(&self) -> &[usize32]  { return self.line_ends.as_slice(); }
}

impl<'code> SrcCode<'code, '_> {
    pub(crate) const fn line_index(&self, column: usize32) -> Index32<usize32> {
        let mut left: usize32 = 0;
        let mut right = self.line_ends.len() as usize32 - 1;

        while left < right {
            let middle = left + (right - left) / 2;

            let line_end = *unsafe { slice::at_unchecked(self.line_ends(), middle as usize) };
            if column <= line_end {
                right = middle;
            }
            else {
                left = middle + 1;
            }
        }

        return Index32::new(left);
    }


    #[must_use] #[inline(always)] #[track_caller]
    pub(crate) const fn line_end(&self, line_index: Index32<usize32>) -> usize32 {
        let line_end = *unsafe { slice::at_unchecked(self.line_ends(), line_index.0 as usize)};
        return line_end;
    }

    #[must_use] #[inline(always)] #[track_caller]
    pub(crate) const fn line_start(&self, line_index: Index32<usize32>) -> usize32 {
        if line_index.0 == 0 { return 0; }

        let next_index = *unsafe { slice::at_unchecked(self.line_ends(), line_index.0 as usize - 1) };
        let Some(next_ch) = text::byte(self.code(), next_index as usize) else {
            return next_index;
        };

        let next_next_index = next_index + 1;
        return match next_ch {
            b'\n' => next_next_index,
            b'\r' => {
                let Some(next_next_ch) = text::byte(self.code(), next_next_index as usize) else {
                    return next_next_index;
                };
                match next_next_ch {
                    b'\n' => next_next_index + 1,
                    _ => next_next_index,
                }
            },
            _ => next_index,
        };
    }

    #[inline(always)] #[track_caller]
    pub(crate) const fn line_span(&self, line_index: Index32<usize32>) -> Span {
        let start = self.line_start(line_index);
        let end = self.line_end(line_index);
        return unsafe { Span::new_unchecked(start, end) };
    }

    #[must_use]
    pub(crate) const fn line_text_with_newlines(&self, line_index: Index32<usize32>) -> &'code str {
        let mut line_start = self.line_start(line_index);
        let line_end = self.line_end(line_index);

        unimplemented!()
        // while (line_end as usize) < self.code().len()
        // let span = self.line_span(line_index);
        // let line_text = self.src_file.text(span.start(), span.end());
        // return line_text;
    }

    #[must_use] #[inline] #[track_caller]
    pub(crate) const fn line_text(&self, line_index: Index32<usize32>) -> &'code str {
        let line_span = self.line_span(line_index);
        let line_text = text::span(self.code(), line_span);
        return line_text;
    }
}


#[derive(Debug)]
#[must_use]
pub enum ErrorKind {
    Io(std::io::Error),
    MustBeAFilePath,
    FileTooBig { max: usize32 },
    CouldNotReadEntireFile,
}


#[derive(Debug)]
#[must_use]
pub struct Error<'path> {
    pub path: &'path Path,
    pub kind: ErrorKind,
}

impl core::fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let message = format!("could not read '{}'", self.path.display());
        let cause = match &self.kind {
            ErrorKind::Io(err) => format!("{err} ({})", err.kind()),
            ErrorKind::MustBeAFilePath => "must be a file path".to_owned(),
            ErrorKind::FileTooBig { max } => {
                format!("file exceeds the size limit of {max} bytes")
            },
            ErrorKind::CouldNotReadEntireFile => "failed to read entire file".to_owned(),
        };

        let error = MsgWithCause { color: MsgSeverity::ERROR_COLOR, kind: ERROR, message: &message, cause: &cause };
        return write!(f, "{error}");
    }
}

#[expect(clippy::missing_trait_methods)]
impl core::error::Error for Error<'_> {}
