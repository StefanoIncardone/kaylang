use crate::{
    cli::Utf8Path,
    error::{BackEndError, BackEndErrorInfo, BackEndErrorKind, ErrorInfo},
};
use std::{io, process::Command};

#[derive(Clone, Copy, Debug)]
pub struct Assembler;

impl Assembler {
    pub fn assemble(
        asm_path: &Utf8Path,
        obj_path: &Utf8Path,
    ) -> Result<(), BackEndError<ErrorKind>> {
        let asm_path_str = asm_path.inner.to_str().unwrap();
        let obj_path_str = obj_path.inner.to_str().unwrap();
        let args = ["-felf64", "-gdwarf", &asm_path_str, "-o", &obj_path_str];

        match Command::new("nasm").args(args).output() {
            Ok(nasm_out) if !nasm_out.status.success() => {
                Err(BackEndError { kind: ErrorKind::Failed { output: nasm_out.stderr } })
            }
            Ok(_) => Ok(()),
            Err(err) => Err(BackEndError { kind: ErrorKind::CouldNotCreateProcess { err } }),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    CouldNotCreateProcess { err: io::Error },
    Failed { output: Vec<u8> },
}

impl ErrorInfo for ErrorKind {
    type Info = BackEndErrorInfo;

    fn info(&self) -> Self::Info {
        let (msg, cause) = match self {
            Self::CouldNotCreateProcess { err } => (
                "could not create assembler process".into(),
                format!("{err} ({kind})", kind = err.kind()).into(),
            ),
            Self::Failed { output } => {
                ("assembler failed".into(), String::from_utf8_lossy(output).into_owned().into())
            }
        };

        Self::Info { msg, cause }
    }
}

impl BackEndErrorKind for ErrorKind {}
