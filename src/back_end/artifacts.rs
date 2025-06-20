use crate::{error::MsgWithCause, ERROR};
use core::fmt::{Display, Write as _};
use std::{
    path::{Path, PathBuf},
    process::Command,
};

const ASM_EXTENSION: &str = "asm";
const OBJ_EXTENSION: &str = "o";
const EXE_EXTENSION: &str = "";

#[derive(Debug)]
pub struct Artifacts {
    pub asm_path: PathBuf,
    pub obj_path: PathBuf,
    pub exe_path: PathBuf,
}

impl Artifacts {
    #[expect(clippy::missing_errors_doc, reason = "the code is the documentation")]
    pub fn new(src_path: &Path, out_path: &Path) -> Result<Self, Error> {
        if src_path.is_dir() {
            return Err(Error::MustBeAFilePath(src_path.to_owned()));
        }

        let src_path_stem = match src_path.file_stem() {
            Some(path_name) => Path::new(path_name),
            None => return Err(Error::SrcPathCannotBeEmpty),
        };

        if out_path.is_file() {
            return Err(Error::MustBeADirectoryPath(out_path.to_owned()));
        }

        let artifacts = if out_path == Path::new("") || out_path == Path::new(".") {
            Self {
                asm_path: src_path_stem.with_extension(ASM_EXTENSION),
                obj_path: src_path_stem.with_extension(OBJ_EXTENSION),
                exe_path: src_path_stem.with_extension(EXE_EXTENSION),
            }
        } else {
            if let Err(err) = std::fs::create_dir_all(out_path) {
                if err.kind() != std::io::ErrorKind::AlreadyExists {
                    return Err(Error::CouldNotCreateOutputDirectory {
                        path: out_path.to_owned(),
                        err,
                    });
                }
            }

            Self {
                asm_path: out_path.join(src_path_stem.with_extension(ASM_EXTENSION)),
                obj_path: out_path.join(src_path_stem.with_extension(OBJ_EXTENSION)),
                exe_path: out_path.join(src_path_stem.with_extension(EXE_EXTENSION)),
            }
        };

        return Ok(artifacts);
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
        _ = linker_command
            .arg(self.obj_path.as_os_str())
            .arg("-o")
            .arg(self.exe_path.as_os_str());
        return linker_command;
    }
}

#[derive(Debug)]
pub enum Error {
    MustBeAFilePath(PathBuf),
    SrcPathCannotBeEmpty,

    MustBeADirectoryPath(PathBuf),
    CouldNotCreateOutputDirectory { path: PathBuf, err: std::io::Error },
}

impl Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut message = String::new();
        let mut cause = String::new();
        match self {
            Self::MustBeAFilePath(path) => {
                _ = write!(message, "invalid '{}' path", path.display());
                _ = write!(cause, "'{}' must be a file path", path.display());
            }
            Self::SrcPathCannotBeEmpty => {
                _ = write!(message, "invalid src path");
                _ = write!(cause, "cannot be empty");
            }
            Self::MustBeADirectoryPath(path) => {
                _ = write!(message, "invalid '{}' path", path.display());
                _ = write!(cause, "'{}' must be a directory path", path.display());
            }
            Self::CouldNotCreateOutputDirectory { path, err } => {
                _ = write!(message, "could not create output directory '{}", path.display());
                _ = write!(cause, "{err} ({})", err.kind());
            }
        }

        let error = MsgWithCause { kind: &ERROR, message: &message, cause: &cause };
        return write!(f, "{error}");
    }
}

#[expect(clippy::missing_trait_methods, reason = "using default implementations")]
impl core::error::Error for Error {}
