use crate::cli::logging::{CAUSE, ERROR};
use std::{
    borrow::Cow,
    fmt::Display,
    io,
    path::{Path, PathBuf},
    process::Command,
};

#[derive(Clone, Copy, Debug)]
pub struct Linker;

impl Linker {
    pub fn link(obj_path: &Path, exe_path: &Path) -> Result<(), Error> {
        let Some(obj_path) = obj_path.to_str() else { return Err(Error::NonUtf8Path { path: obj_path.to_path_buf() }) };
        let Some(exe_path) = exe_path.to_str() else { return Err(Error::NonUtf8Path { path: exe_path.to_path_buf() }) };

        match Command::new("ld").args([obj_path, "-o", exe_path]).output() {
            Ok(ld_out) if !ld_out.status.success() => Err(Error::Failed { output: ld_out.stderr }),
            Ok(_) => Ok(()),
            Err(err) => Err(Error::CouldNotCreateProcess { err }),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    NonUtf8Path { path: PathBuf },
    CouldNotCreateProcess { err: io::Error },
    Failed { output: Vec<u8> },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (msg, cause): (Cow<'_, str>, Cow<'_, str>) = match self {
            Self::NonUtf8Path { path } => {
                ("invalid path".into(), format!("'{path}' contains non UTF8 characters", path = path.display()).into())
            }
            Self::CouldNotCreateProcess { err } => {
                ("could not create linker process".into(), format!("{err} ({kind})", kind = err.kind()).into())
            }
            Self::Failed { output } => ("linker failed".into(), String::from_utf8_lossy(output).into_owned().into()),
        };

        write!(
            f,
            "{ERROR}: {msg}\
            \n{CAUSE}: {cause}"
        )
    }
}

impl std::error::Error for Error {}
