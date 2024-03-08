use crate::error::{ErrorInfo, SrcFileError, SrcFileErrorInfo, SrcFileErrorKind};
use std::{
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
        let mut right = src.lines.len();
        while left < right {
            let middle = left + (right - left) / 2;
            if col < src.lines[middle].end {
                right = middle;
            } else {
                left = middle + 1;
            }
        }

        Self::new_with_line_index(src, left, col)
    }

    pub(crate) fn new_with_line_index(src: &'src SrcFile, line_index: usize, col: usize) -> (Self, &'src str) {
        let line = &src.lines[line_index];
        let line_text = &src.code[line.start..line.end];
        let target_col = col - line.start;
        let mut display_col = 0;
        for (index, _) in line_text.char_indices() {
            display_col += 1;
            if index == target_col {
                break;
            }
        }

        (Self { line: line_index + 1, col: display_col }, line_text)
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
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self, Error> {
        let path = path.as_ref().to_path_buf();

        let file = match File::open(&path) {
            Ok(f) => f,
            Err(err) => return Err(SrcFileError { kind: ErrorKind::CouldNotOpen { err, path } }),
        };

        let file_len = match file.metadata() {
            Ok(metadata) if metadata.is_file() => metadata.len() as usize,
            Ok(_) => return Err(SrcFileError { kind: ErrorKind::ExpectedFile { path } }),
            Err(err) => return Err(SrcFileError { kind: ErrorKind::CouldNotReadMetadata { err, path } }),
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
                Err(err) => return Err(SrcFileError { kind: ErrorKind::CouldNotReadContents { err, path } }),
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
}

#[derive(Debug)]
pub enum ErrorKind {
    CouldNotOpen { err: io::Error, path: PathBuf },
    ExpectedFile { path: PathBuf },
    CouldNotReadMetadata { err: io::Error, path: PathBuf },
    CouldNotReadContents { err: io::Error, path: PathBuf },
}

impl ErrorInfo for ErrorKind {
    type Info = SrcFileErrorInfo;

    fn info(&self) -> Self::Info {
        let (msg, cause) = match self {
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

        Self::Info { msg, cause }
    }
}

impl SrcFileErrorKind for ErrorKind {}

#[deprecated(since = "0.5.3", note = "will be removed to allow for more explicit function signatures")]
pub type Error = SrcFileError<ErrorKind>;
