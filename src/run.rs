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
        let exe_path = match exe_path.to_str() {
            Some(exe_path) => Path::new(".").join(exe_path),
            None => return Err(Error::NonUtf8Path { path: exe_path.to_path_buf() }),
        };

        let mut executable = match Command::new(&exe_path).spawn() {
            Ok(executable) => executable,
            Err(err) => return Err(Error::CouldNotCreateExecutableProcess { err, path: exe_path }),
        };

        match executable.wait() {
            Ok(_) => Ok(()),
            Err(err) => Err(Error::CouldNotRunExecutable { err, path: exe_path }),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    NonUtf8Path { path: PathBuf },
    CouldNotCreateExecutableProcess { err: io::Error, path: PathBuf },
    CouldNotRunExecutable { err: io::Error, path: PathBuf },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (msg, cause): (Cow<'static, str>, Cow<'static, str>) = match self {
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

        write!(
            f,
            "{ERROR}: {msg}\
            \n{CAUSE}: {cause}"
        )
    }
}

impl std::error::Error for Error {}
