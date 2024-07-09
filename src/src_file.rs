use crate::{CAUSE, ERROR};
use std::{
    fmt::Display,
    fs::File,
    io::{self, BufRead, BufReader},
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Line {
    /// inclusive
    pub(crate) start: usize,

    /// not inclusive
    pub(crate) end: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl<'src> Position {
    pub(crate) fn new(src: &'src SrcFile, col: usize) -> (Self, &'src str) {
        let mut left = 0;
        let mut right = src.lines.len() - 1;
        while left < right {
            #[allow(clippy::integer_division)] // it's intended to lose precision
            let middle = left + (right - left) / 2;
            if col < src.lines[middle].end {
                right = middle;
            } else {
                left = middle + 1;
            }
        }

        // converting from column offset to display offset (useful for utf8 characters)
        let line = &src.lines[left];
        let line_text = &src.code[line.start..line.end];
        let target_col = col - line.start;
        let mut display_col = 0;
        for (index, _) in line_text.char_indices() {
            display_col += 1;
            if index == target_col {
                break;
            }
        }

        return (Self { line: left + 1, col: display_col }, line_text);
    }
}

#[derive(Debug)]
pub struct SrcFile {
    pub(crate) path: PathBuf,
    pub(crate) code: String,
    pub(crate) lines: Vec<Line>,
}

impl SrcFile {
    // TODO(stefano): replace indentation tabs with spaces
    pub fn load(path: &Path) -> Result<Self, Error> {
        let file = match File::open(path) {
            Ok(f) => f,
            Err(err) => {
                return Err(Error {
                    kind: ErrorKind::CouldNotOpen { path: path.to_owned() },
                    cause: ErrorCause::IoError(err),
                });
            }
        };

        let file_len = match file.metadata() {
            Ok(metadata) => metadata.len() as usize,
            Err(err) => {
                return Err(Error {
                    kind: ErrorKind::CouldNotReadMetadata { path: path.to_owned() },
                    cause: ErrorCause::IoError(err),
                });
            }
        };

        // plus one to account for a possible phantom newline at the end
        let mut code = String::with_capacity(file_len + 1);
        let mut lines = Vec::<Line>::new();
        let mut start = 0;
        let mut src = BufReader::new(file);

        loop {
            let mut chars_read = match src.read_line(&mut code) {
                Ok(0) => break,
                Ok(read) => read,
                Err(err) => {
                    return Err(Error {
                        kind: ErrorKind::CouldNotReadContents { path: path.to_owned() },
                        cause: ErrorCause::IoError(err),
                    });
                }
            };

            let mut end = code.len() - 1;
            if end > start {
                let code_bytes = unsafe { code.as_mut_vec() };

                if let cr @ b'\r' = &mut code_bytes[end - 1] {
                    *cr = b'\n';

                    unsafe {
                        code_bytes.set_len(end);
                    }

                    end -= 1;
                    chars_read -= 1;
                }
            }

            lines.push(Line { start, end });
            start += chars_read;
        }

        // it will make lexing simpler
        if !code.is_empty() {
            let last_char = code.len() - 1;
            if code.as_bytes()[last_char] != b'\n' {
                code.push('\n');
                let last_line = lines.len() - 1;
                lines[last_line].end += 1;
            }
        }

        return Ok(Self { path: path.to_owned(), code, lines });
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    CouldNotOpen { path: PathBuf },
    CouldNotReadMetadata { path: PathBuf },
    CouldNotReadContents { path: PathBuf },
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::CouldNotOpen { path } => write!(f, "could not open '{}'", path.display()),
            Self::CouldNotReadMetadata { path } => {
                write!(f, "could not read metadata of '{}'", path.display())
            }
            Self::CouldNotReadContents { path } => {
                write!(f, "could not read contents of '{}'", path.display())
            }
        };
    }
}

#[derive(Debug)]
pub enum ErrorCause {
    IoError(io::Error),
}

impl Display for ErrorCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::IoError(err) => write!(f, "{err} ({})", err.kind()),
        };
    }
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub cause: ErrorCause,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            "{ERROR}: {msg}\
            \n{CAUSE}: {cause}",
            msg = self.kind,
            cause = self.cause
        );
    }
}
