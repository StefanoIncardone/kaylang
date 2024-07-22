use crate::{CAUSE, ERROR};
use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Line {
    /// inclusive
    pub start: usize,

    /// not inclusive, points to the last non-newline character
    pub end: usize,
}

// TODO(stefano): include line text
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub col: usize,
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
        let code = match std::fs::read_to_string(path) {
            Ok(code) => code,
            Err(err) => return Err(Error { path: path_buf, cause: err }),
        };

        let mut lines = Vec::<Line>::new();
        let mut start = 0;
        let mut current_ascii_index = 0;

        let code_bytes = code.as_bytes();
        while current_ascii_index < code_bytes.len() {
            match code_bytes[current_ascii_index] {
                b'\n' => {
                    // we reached the end of the line on a LF (\n)
                    lines.push(Line { start, end: current_ascii_index });
                    start = current_ascii_index + 1;
                }
                b'\r' => {
                    let Some(possible_new_line) = code_bytes.get(current_ascii_index + 1) else {
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

        if !code.is_empty() && code_bytes[current_ascii_index - 1] != b'\n' {
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
    pub fn position(&self, col: usize) -> (Position, &str) {
        let mut left = 0;
        let mut right = self.lines.len() - 1;
        while left < right {
            #[allow(clippy::integer_division)] // it's intended to lose precision
            let middle = left + (right - left) / 2;
            if col < self.lines[middle].end {
                right = middle;
            } else {
                left = middle + 1;
            }
        }

        // converting from column offset to display offset (useful for utf8 characters)
        let line = &self.lines[left];
        let line_text = &self.code[line.start..line.end];
        let target_col = col - line.start;
        let mut display_col = 0;
        for (index, _) in line_text.char_indices() {
            display_col += 1;
            if index == target_col {
                break;
            }
        }

        return (Position { line: left + 1, col: display_col }, line_text);
    }
}

#[derive(Debug)]
pub struct Error {
    pub path: PathBuf,
    pub cause: std::io::Error,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            "{ERROR}: could not read '{file_path}'\
            \n{CAUSE}: {err} ({io_err})",
            file_path = self.path.display(),
            err = self.cause,
            io_err = self.cause.kind()
        );
    }
}
