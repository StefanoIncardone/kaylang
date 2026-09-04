/* TODO(stefano):
fix allocation of arrays of 0 items when thet will be allowed by either
- allow for zero size values
- place padding between elements (i.e. disallow zero size values by making them at least 1 byte long)
*/
// IDEA(stefano): represent false as 0 and true as non-0 instead of 1

pub mod linux;
pub mod nasm;

use cfg_if::cfg_if;

use crate::{ERROR, error::MsgWithCause};
use core::fmt::{Display, Write as _};
use std::{
    path::{Path, PathBuf}, process::Command,
};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArtifactsExtensions {
    pub asm: &'static str,
    pub obj: &'static str,
    pub exe: &'static str,
}

impl ArtifactsExtensions {
    #[must_use]
    #[inline(always)]
    pub const fn clone_const(&self) -> Self {
        return Self { ..*self };
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub enum Platform {
    #[default]
    Auto,
    Linux,
}

impl Platform {
    #[must_use]
    #[inline]
    pub const fn extensions(self) -> ArtifactsExtensions {
        return match self {
            Self::Auto => {
                cfg_if! {
                    if #[cfg(target_os = "linux")] {
                        Self::Linux.extensions()
                    }
                    else {
                        compile_error!("only linux is supported for now");
                    }
                }
            }
            Self::Linux => linux::EXTENSIONS.clone_const(),
        }
    }

    #[must_use]
    #[inline]
    pub fn assembler(self, artifacts: &Artifacts) -> Command {
        return match self {
            Self::Auto => {
                cfg_if! {
                    if #[cfg(target_os = "linux")] {
                        Self::Linux.assembler(artifacts)
                    }
                    else {
                        compile_error!("only linux is supported for now");
                    }
                }
            }
            Self::Linux => nasm_assembler(artifacts),
        };
    }

    #[must_use]
    #[inline]
    pub fn linker(self, artifacts: &Artifacts) -> Command {
        return match self {
            Self::Auto => {
                cfg_if! {
                    if #[cfg(target_os = "linux")] {
                        Self::Linux.linker(artifacts)
                    }
                    else {
                        compile_error!("only linux is supported for now");
                    }
                }
            }
            Self::Linux => ld_linker(artifacts),
        };
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Artifacts {
    pub asm_path: PathBuf,
    pub obj_path: PathBuf,
    pub exe_path: PathBuf,
}

impl Artifacts {
    // BUG(stefano): running `run-asm tests/test.kay -o=out` works accidentally because
    // `out/test.asm` exists, `run-asm` should directly use `tests/test.kay`
    // IDEA(stefano): extract Command::Check/Compile/Run args to structs and have them calculate
    // the relevant paths
    #[expect(clippy::missing_errors_doc)]
    pub fn new(src_path: &Path, out_path: &Path, extensions: &ArtifactsExtensions) -> Result<Self, Error> {
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

        if out_path == Path::new("") || out_path == Path::new(".") {
            let artifacts = Self {
                asm_path: src_path_stem.with_extension(extensions.asm),
                obj_path: src_path_stem.with_extension(extensions.obj),
                exe_path: src_path_stem.with_extension(extensions.exe),
            };
            return Ok(artifacts);
        }

        if let Err(err) = std::fs::create_dir_all(out_path)
            && err.kind() != std::io::ErrorKind::AlreadyExists
        {
            return Err(Error::CouldNotCreateOutputDirectory { path: out_path.to_owned(), err });
        }

        let artifacts = Self {
            asm_path: out_path.join(src_path_stem.with_extension(extensions.asm)),
            obj_path: out_path.join(src_path_stem.with_extension(extensions.obj)),
            exe_path: out_path.join(src_path_stem.with_extension(extensions.exe)),
        };
        return Ok(artifacts);
    }
}

#[must_use]
pub fn nasm_assembler(artifacts: &Artifacts) -> Command {
    let mut assembler_command = Command::new("nasm");
    _ = assembler_command
        .arg("-felf64")
        .arg("-gdwarf")
        .arg("-Werror")
        .arg("-Wall")
        .arg("-W-error=reloc")
        .arg(artifacts.asm_path.as_os_str())
        .arg("-o")
        .arg(artifacts.obj_path.as_os_str());
    return assembler_command;
}

#[must_use]
pub fn ld_linker(artifacts: &Artifacts) -> Command {
    let mut linker_command = Command::new("ld");
    _ = linker_command
        .arg(artifacts.obj_path.as_os_str())
        .arg("-o")
        .arg(artifacts.exe_path.as_os_str());
    return linker_command;
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
            },
            Self::SrcPathCannotBeEmpty => {
                _ = write!(message, "invalid src path");
                _ = write!(cause, "cannot be empty");
            },
            Self::MustBeADirectoryPath(path) => {
                _ = write!(message, "invalid '{}' path", path.display());
                _ = write!(cause, "'{}' must be a directory path", path.display());
            },
            Self::CouldNotCreateOutputDirectory { path, err } => {
                _ = write!(message, "could not create output directory '{}", path.display());
                _ = write!(cause, "{err} ({})", err.kind());
            },
        }

        let error = MsgWithCause { kind: &ERROR, message: &message, cause: &cause };
        return write!(f, "{error}");
    }
}

#[expect(clippy::missing_trait_methods)]
impl core::error::Error for Error {}
