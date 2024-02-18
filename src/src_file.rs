use crate::logging::{CAUSE, ERROR};
use std::{
    borrow::Cow,
    fmt::Display,
    fs::File,
    io::{self, BufRead, BufReader},
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, Copy)]
pub(crate) struct Line {
    pub(crate) start: usize, // inclusive
    pub(crate) end: usize,   // not inclusive
}

#[derive(Debug, Clone, Copy)]
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
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self, Error> {
        let path = path.as_ref().to_path_buf();

        let file = match File::open(&path) {
            Ok(f) => f,
            Err(err) => return Err(Error::CouldNotOpen { err, path }),
        };

        let file_len = match file.metadata() {
            Ok(metadata) if metadata.is_file() => metadata.len() as usize,
            Ok(_) => return Err(Error::ExpectedFile { path }),
            Err(err) => return Err(Error::CouldNotReadMetadata { err, path }),
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
                Err(err) => return Err(Error::CouldNotReadContents { err, path }),
            };

            let mut end = code.len() - 1;
            if end > start {
                if let cr @ b'\r' = &mut unsafe { code.as_bytes_mut() }[end - 1] {
                    *cr = b'\n';
                    unsafe { code.as_mut_vec().set_len(end) };
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

        Ok(Self { path, code, lines })
    }

    pub(crate) fn position(&self, col: usize) -> Position {
        let mut left = 0;
        let mut right = self.lines.len();
        while left < right {
            let middle = left + (right - left) / 2;
            if col < self.lines[middle].end {
                right = middle;
            } else {
                left = middle + 1;
            }
        }

        Position { line: left + 1, col: col + 1 - self.lines[left].start }
    }

    pub(crate) fn line_text(&self, position: Position) -> &str {
        let line = &self.lines[position.line - 1];
        &self.code[line.start..line.end]
    }
}

#[derive(Debug)]
pub enum Error {
    CouldNotOpen { err: io::Error, path: PathBuf },
    ExpectedFile { path: PathBuf },
    CouldNotReadMetadata { err: io::Error, path: PathBuf },
    CouldNotReadContents { err: io::Error, path: PathBuf },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (msg, cause): (Cow<'_, str>, Cow<'_, str>) = match self {
            Self::CouldNotOpen { err, path } => (
                format!("could not open '{path}'", path = path.display()).into(),
                format!("{err} ({kind})", kind = err.kind()).into(),
            ),
            Self::ExpectedFile { path } => (
                format!("invalid path '{path}'", path = path.display()).into(),
                "expected a file but got a directory".into(),
            ),
            Self::CouldNotReadMetadata { err, path } => (
                format!("could not read metadata of '{path}'", path = path.display()).into(),
                format!("{err} ({kind})", kind = err.kind()).into(),
            ),
            Self::CouldNotReadContents { err, path } => (
                format!("could not read contents of '{path}'", path = path.display()).into(),
                format!("{err} ({kind})", kind = err.kind()).into(),
            ),
        };

        write!(
            f,
            "{ERROR}: {msg}\
            \n{CAUSE}: {cause}",
        )
    }
}

impl std::error::Error for Error {}
