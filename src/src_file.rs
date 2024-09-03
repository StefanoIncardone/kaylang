use crate::{CAUSE, ERROR};
use core::fmt::Display;
use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

#[allow(non_camel_case_types)]
pub type offset = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// inclusive
    pub start: offset,

    /// not inclusive
    pub end: offset,
}

pub type Line = Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: offset,
    // IDEA(stefano): restrict the max line length to 255/511/1023/2047/4095
    pub col: offset,
}

#[derive(Debug)]
pub struct SrcFile {
    pub(crate) path: PathBuf,
    pub(crate) code: String,
    pub(crate) lines: Vec<Line>,
}

impl SrcFile {
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
                if file_len > offset::MAX as u64 {
                    return Err(Error { path: path_buf, kind: ErrorKind::FileTooBig { max: offset::MAX } });
                }
                file_len as offset
            }
            Err(err) => return Err(Error { path: path_buf, kind: ErrorKind::Io(err) }),
        };

        let code = {
            let mut code = String::with_capacity(file_len as usize);
            let bytes_read = match file.read_to_string(&mut code) {
                Ok(bytes_read) => bytes_read as offset,
                Err(err) => return Err(Error { path: path_buf, kind: ErrorKind::Io(err) }),
            };

            if bytes_read != file_len {
                return Err(Error { path: path_buf, kind: ErrorKind::CouldNotReadEntireFile });
            }

            code
        };

        let mut lines = Vec::<Line>::new();
        let mut start = 0;
        let mut current_ascii_index: offset = 0;

        let code_bytes = code.as_bytes();
        let code_bytes_len = code_bytes.len() as offset;
        while current_ascii_index < code_bytes_len {
            match code_bytes[current_ascii_index as usize] {
                b'\n' => {
                    // we reached the end of the line on a LF (\n)
                    lines.push(Line { start, end: current_ascii_index });
                    start = current_ascii_index + 1;
                }
                b'\r' => {
                    let Some(possible_new_line) = code_bytes.get(current_ascii_index as usize + 1)
                    else {
                        // we reached the end of the file on a stray \r
                        lines.push(Line { start, end: current_ascii_index });
                        break;
                    };

                    if *possible_new_line == b'\n' {
                        // we reached the end of the line on a CRLF (\r\n)
                        lines.push(Line { start, end: current_ascii_index });
                        current_ascii_index += 1;
                        start = current_ascii_index + 1;
                    }
                }
                _ => {}
            }

            current_ascii_index += 1;
        }

        if !code.is_empty() && code_bytes[current_ascii_index as usize - 1] != b'\n' {
            // we reached the end of the file on a line without a trailing \n
            lines.push(Line { start, end: current_ascii_index });
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
    pub fn position(&self, col: offset) -> Position {
        let mut left: offset = 0;
        let mut right = self.lines.len() as offset - 1;
        while left < right {
            #[allow(clippy::integer_division)] // it's intended to lose precision
            let middle = left + (right - left) / 2;
            if col < self.lines[middle as usize].end {
                right = middle;
            } else {
                left = middle + 1;
            }
        }

        // converting from column offset to display offset (useful for utf8 characters)
        let line = &self.lines[left as usize];
        let line_text = &self.code[line.start as usize..line.end as usize];
        let target_col = col - line.start;
        let mut display_col = 0;
        for (index, _) in line_text.char_indices() {
            display_col += 1;
            if index == target_col as usize {
                break;
            }
        }

        return Position { line: left + 1, col: display_col };
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Io(std::io::Error),
    MustBeAFilePath,
    FileTooBig { max: offset },
    CouldNotReadEntireFile,
}

#[derive(Debug)]
pub struct Error {
    pub path: PathBuf,
    pub kind: ErrorKind,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let cause = match &self.kind {
            ErrorKind::Io(err) => format!("{err} ({})", err.kind()),
            ErrorKind::MustBeAFilePath => "must be a file path".to_owned(),
            ErrorKind::FileTooBig { max } => {
                format!("file exceeds the size limit of {max} bytes")
            }
            ErrorKind::CouldNotReadEntireFile => "failed to read entire file".to_owned(),
        };

        return write!(
            f,
            "{ERROR}: could not read '{file_path}'\
            \n{CAUSE}: {cause}",
            file_path = self.path.display(),
        );
    }
}
