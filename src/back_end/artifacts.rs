use crate::{error::MsgWithCause, ERROR};
use core::fmt::{Display, Write as _};
use std::{
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
    #[must_use]
    pub fn new(src_path: &Path) -> Self {
        let src_path_stem = match src_path.file_stem() {
            Some(path_name) => PathBuf::from(path_name),
            None => unreachable!("file stem should always be present"),
        };

        return Self {
            asm_path: src_path_stem.with_extension("asm"),
            obj_path: src_path_stem.with_extension("o"),
            exe_path: src_path_stem,
        };
    }

    pub fn new_with_out_path(src_path: &Path, out_path: &Path) -> Result<Self, Error> {
        if out_path.is_file() {
            return Err(Error::MustBeADirectoryPath(out_path.to_owned()));
        }

        match std::fs::create_dir_all(out_path) {
            Ok(()) => {}
            Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => {}
            Err(err) => {
                return Err(Error::CouldNotCreateOutputDirectory { path: out_path.to_owned(), err })
            }
        }

        let src_path_stem = match src_path.file_stem() {
            Some(path_name) => PathBuf::from(path_name),
            None => unreachable!("file stem should always be present"),
        };

        return Ok(Self {
            asm_path: out_path.join(&src_path_stem).with_extension("asm"),
            obj_path: out_path.join(&src_path_stem).with_extension("o"),
            exe_path: out_path.join(src_path_stem),
        });
    }

    #[must_use]
    pub fn assembler(&self) -> Command {
        let mut assembler_command = Command::new("nasm");
        _ = assembler_command
            .arg("-felf64")
            .arg("-gdwarf")
            .arg("-Werror")
            .arg("-Wall")
            .arg("-W-error=reloc")
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
}

#[derive(Debug)]
pub enum Error {
    CouldNotCreateOutputDirectory { path: PathBuf, err: std::io::Error },
    MustBeADirectoryPath(PathBuf),
}

impl Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut message = String::new();
        let mut cause = String::new();
        match self {
            Self::CouldNotCreateOutputDirectory { path, err } => {
                _ = write!(message, "could not create output directory '{}", path.display());
                _ = write!(cause, "{err} ({})", err.kind());
            }
            Self::MustBeADirectoryPath(path) => {
                _ = write!(message, "invalid '{}' path", path.display());
                _ = write!(cause, "'{}' must be a directory path", path.display());
            }
        }

        let error = MsgWithCause { kind: &ERROR, message: &message, cause: &cause };
        return write!(f, "{error}");
    }
}

#[expect(clippy::missing_trait_methods, reason = "using core::error::Error default implementations")]
impl core::error::Error for Error {}
