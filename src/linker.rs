use crate::error::{BackEndError, BackEndErrorInfo, BackEndErrorKind, ErrorInfo};
use std::{
    io,
    path::{Path, PathBuf},
    process::Command,
};

#[derive(Clone, Copy, Debug)]
pub struct Linker;

impl Linker {
    pub fn link(obj_path: &Path, exe_path: &Path) -> Result<(), Error> {
        let Some(obj_path) = obj_path.to_str() else {
            return Err(BackEndError { kind: ErrorKind::NonUtf8Path { path: obj_path.to_path_buf() } });
        };

        let Some(exe_path) = exe_path.to_str() else {
            return Err(BackEndError { kind: ErrorKind::NonUtf8Path { path: exe_path.to_path_buf() } });
        };

        match Command::new("ld").args([obj_path, "-o", exe_path]).output() {
            Ok(ld_out) if !ld_out.status.success() => {
                Err(BackEndError { kind: ErrorKind::Failed { output: ld_out.stderr } })
            }
            Ok(_) => Ok(()),
            Err(err) => Err(BackEndError { kind: ErrorKind::CouldNotCreateProcess { err } }),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    NonUtf8Path { path: PathBuf },
    CouldNotCreateProcess { err: io::Error },
    Failed { output: Vec<u8> },
}

impl ErrorInfo for ErrorKind {
    type Info = BackEndErrorInfo;

    fn info(&self) -> Self::Info {
        let (msg, cause) = match self {
            Self::NonUtf8Path { path } => {
                ("invalid path".into(), format!("'{path}' contains non UTF8 characters", path = path.display()).into())
            }
            Self::CouldNotCreateProcess { err } => {
                ("could not create linker process".into(), format!("{err} ({kind})", kind = err.kind()).into())
            }
            Self::Failed { output } => ("linker failed".into(), String::from_utf8_lossy(output).into_owned().into()),
        };

        Self::Info { msg, cause }
    }
}

impl BackEndErrorKind for ErrorKind {}

#[deprecated(since = "0.5.3", note = "will be removed to allow for more explicit function signatures")]
pub type Error = BackEndError<ErrorKind>;
