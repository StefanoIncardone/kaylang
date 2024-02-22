use super::{BackEndError, BackEndErrorInfo, BackEndErrorKindInfo};
use std::{
    io,
    path::{Path, PathBuf},
    process::Command,
};

#[derive(Clone, Copy, Debug)]
pub struct Assembler;

impl Assembler {
    pub fn assemble(asm_path: &Path, obj_path: &Path) -> Result<(), Error> {
        let Some(asm_path) = asm_path.to_str() else {
            return Err(Error { kind: ErrorKind::NonUtf8Path { path: asm_path.to_path_buf() } });
        };

        let Some(obj_path) = obj_path.to_str() else {
            return Err(Error { kind: ErrorKind::NonUtf8Path { path: obj_path.to_path_buf() } });
        };

        match Command::new("nasm").args(["-felf64", "-gdwarf", asm_path, "-o", obj_path]).output() {
            Ok(nasm_out) if !nasm_out.status.success() => {
                Err(Error { kind: ErrorKind::Failed { output: nasm_out.stderr } })
            }
            Ok(_) => Ok(()),
            Err(err) => Err(Error { kind: ErrorKind::CouldNotCreateProcess { err } }),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    NonUtf8Path { path: PathBuf },
    CouldNotCreateProcess { err: io::Error },
    Failed { output: Vec<u8> },
}

impl BackEndErrorKindInfo for ErrorKind {
    fn info(&self) -> BackEndErrorInfo {
        let (msg, cause) = match self {
            Self::NonUtf8Path { path } => {
                ("invalid path".into(), format!("'{path}' contains non UTF8 characters", path = path.display()).into())
            }
            Self::CouldNotCreateProcess { err } => {
                ("could not create assembler process".into(), format!("{err} ({kind})", kind = err.kind()).into())
            }
            Self::Failed { output } => ("assembler failed".into(), String::from_utf8_lossy(output).into_owned().into()),
        };

        BackEndErrorInfo { msg, cause }
    }
}

pub type Error = BackEndError<ErrorKind>;
