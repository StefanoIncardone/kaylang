use crate::logging::{CAUSE, ERROR};
use std::{borrow::Cow, fmt::Display, io, path::Path, process::Command};

#[derive(Clone, Copy, Debug)]
pub struct Assembler;

impl Assembler {
    pub fn assemble(asm_path: &Path, obj_path: &Path) -> Result<(), Error> {
        let nasm_args = ["-felf64", "-gdwarf", asm_path.to_str().unwrap(), "-o", obj_path.to_str().unwrap()];
        match Command::new("nasm").args(nasm_args).output() {
            Ok(nasm_out) if !nasm_out.status.success() => Err(Error::Failed { output: nasm_out.stderr }),
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
                ("could not create assembler process".into(), format!("{} ({})", err, err.kind()).into())
            }
            Self::Failed { output } => ("assembler failed".into(), String::from_utf8_lossy(output).into_owned().into()),
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
