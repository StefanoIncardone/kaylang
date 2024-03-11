use crate::error::{BackEndError, BackEndErrorInfo, BackEndErrorKind, ErrorInfo};
use std::{
    io,
    path::{Path, PathBuf},
    process::Command,
};

#[derive(Debug)]
pub struct Run;

impl Run {
    pub fn run(exe_path: &Path) -> Result<(), BackEndError<ErrorKind>> {
        let exe_path = match exe_path.to_str() {
            Some(exe_path) => Path::new(".").join(exe_path),
            None => return Err(BackEndError { kind: ErrorKind::NonUtf8Path { path: exe_path.to_path_buf() } }),
        };

        let mut executable = match Command::new(&exe_path).spawn() {
            Ok(executable) => executable,
            Err(err) => {
                return Err(BackEndError { kind: ErrorKind::CouldNotCreateExecutableProcess { err, path: exe_path } })
            }
        };

        match executable.wait() {
            Ok(_) => Ok(()),
            Err(err) => Err(BackEndError { kind: ErrorKind::CouldNotRunExecutable { err, path: exe_path } }),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    NonUtf8Path { path: PathBuf },
    CouldNotCreateExecutableProcess { err: io::Error, path: PathBuf },
    CouldNotRunExecutable { err: io::Error, path: PathBuf },
}

impl ErrorInfo for ErrorKind {
    type Info = BackEndErrorInfo;

    fn info(&self) -> Self::Info {
        let (msg, cause) = match self {
            Self::NonUtf8Path { path } => {
                ("invalid path".into(), format!("'{path}' contains non UTF8 characters", path = path.display()).into())
            }
            Self::CouldNotCreateExecutableProcess { err, path } => (
                format!("could not create executable process '{path}'", path = path.display()).into(),
                format!("{err} ({kind})", kind = err.kind()).into(),
            ),
            Self::CouldNotRunExecutable { err, path } => (
                format!("could not run executable process '{path}'", path = path.display()).into(),
                format!("{err} ({kind})", kind = err.kind()).into(),
            ),
        };

        Self::Info { msg, cause }
    }
}

impl BackEndErrorKind for ErrorKind {}
