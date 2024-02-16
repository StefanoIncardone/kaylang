use crate::logging::{CAUSE, ERROR};
use std::{borrow::Cow, fmt::Display, io, path::Path, process::Command};

#[derive(Clone, Copy, Debug)]
pub struct Linker;

impl Linker {
    pub fn link(obj_path: &Path, exe_path: &Path) -> Result<(), Error> {
        let ld_args = [obj_path.to_str().unwrap(), "-o", exe_path.to_str().unwrap()];
        match Command::new("ld").args(ld_args).output() {
            Ok(ld_out) if !ld_out.status.success() => Err(Error::Failed { output: ld_out.stderr }),
            Ok(_) => Ok(()),
            Err(err) => Err(Error::CouldNotCreateProcess { err }),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    CouldNotCreateProcess { err: io::Error },
    Failed { output: Vec<u8> },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (msg, cause): (Cow<'static, str>, Cow<'static, str>) = match self {
            Self::CouldNotCreateProcess { err } => {
                ("could not create linker process".into(), format!("{} ({})", err, err.kind()).into())
            }
            Self::Failed { output } => ("linker failed".into(), String::from_utf8_lossy(output).into_owned().into()),
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
