use crate::{src_file::SrcFile, CAUSE, ERROR};
use std::{
    fmt::Display,
    io,
    path::{Path, PathBuf},
    process::Command,
};

#[derive(Debug)]
pub struct Artifacts {
    pub asm_path: PathBuf,
    pub obj_path: PathBuf,
    pub exe_path: PathBuf,
}

impl Artifacts {
    pub fn new(src: &SrcFile, out_path: Option<&Path>) -> Result<Self, Error> {
        let mut asm_path: PathBuf = src.path.with_extension("asm");
        let mut obj_path: PathBuf = src.path.with_extension("o");
        let mut exe_path: PathBuf = src.path.with_extension("");

        if let Some(out_path_buf) = out_path {
            match std::fs::create_dir_all(out_path_buf) {
                Ok(()) => {}
                Err(err) if err.kind() == io::ErrorKind::AlreadyExists => {}
                Err(err) => {
                    return Err(Error {
                        kind: ErrorKind::CouldNotCreateOutputDirectory {
                            path: out_path_buf.to_owned(),
                        },
                        cause: ErrorCause::IoError(err),
                    });
                }
            }

            let asm_file_name = unsafe { asm_path.file_name().unwrap_unchecked() };
            let obj_file_name = unsafe { obj_path.file_name().unwrap_unchecked() };
            let exe_file_name = unsafe { exe_path.file_name().unwrap_unchecked() };

            asm_path = out_path_buf.join(asm_file_name);
            obj_path = out_path_buf.join(obj_file_name);
            exe_path = out_path_buf.join(exe_file_name);
        }

        return Ok(Self { asm_path, obj_path, exe_path });
    }

    #[must_use]
    pub fn assembler(&self) -> Command {
        let mut assembler_command = Command::new("nasm");
        _ = assembler_command
            .arg("-felf64")
            .arg("-gdwarf")
            .arg("-Werror")
            .arg("-Wall")
            .arg(self.asm_path.as_os_str())
            .arg("-o")
            .arg(self.obj_path.as_os_str());
        return assembler_command;
    }

    #[must_use]
    pub fn linker(&self) -> Command {
        let mut linker_command = Command::new("ld");
        _ = linker_command.arg(self.obj_path.as_os_str()).arg("-o").arg(self.exe_path.as_os_str());
        return linker_command;
    }

    #[must_use]
    pub fn runner(&self) -> Command {
        return Command::new(self.exe_path.as_os_str());
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    CouldNotCreateOutputDirectory { path: PathBuf },
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::CouldNotCreateOutputDirectory { path } => {
                write!(f, "could not create output directory '{}", path.display())
            }
        };
    }
}

#[derive(Debug)]
pub enum ErrorCause {
    IoError(io::Error),
}

impl Display for ErrorCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::IoError(err) => write!(f, "{err} ({})", err.kind()),
        };
    }
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub cause: ErrorCause,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            "{ERROR}: {msg}\
            \n{CAUSE}: {cause}",
            msg = self.kind,
            cause = self.cause
        );
    }
}