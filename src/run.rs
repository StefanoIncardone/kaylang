use crate::logging::{CAUSE, ERROR};
use std::{
    borrow::Cow,
    fmt::Display,
    io,
    path::{Path, PathBuf},
    process::Command,
};

#[derive(Debug)]
pub struct Run;

impl Run {
    pub fn run(exe_path: &Path) -> Result<(), Error> {
        let mut executable = match Command::new(Path::new(".").join(exe_path)).spawn() {
            Ok(executable) => executable,
            Err(err) => return Err(Error::CouldNotCreateExecutableProcess { err, path: exe_path.to_path_buf() }),
        };

        match executable.wait() {
            Ok(_) => Ok(()),
            Err(err) => Err(Error::CouldNotRunExecutable { err, path: exe_path.to_path_buf() }),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    CouldNotCreateExecutableProcess { err: io::Error, path: PathBuf },
    CouldNotRunExecutable { err: io::Error, path: PathBuf },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (msg, cause): (Cow<'static, str>, Cow<'static, str>) = match self {
            Self::CouldNotCreateExecutableProcess { err, path } => (
                format!("could not create executable process '{}'", path.display()).into(),
                format!("{} ({})", err, err.kind()).into(),
            ),
            Self::CouldNotRunExecutable { err, path } => (
                format!("could not run executable process '{}'", path.display()).into(),
                format!("{} ({})", err, err.kind()).into(),
            ),
        };

        write!(
            f,
            "{}: {}\
            \n{}: {}",
            ERROR, msg, CAUSE, cause
        )
    }
}

impl std::error::Error for Error {}
