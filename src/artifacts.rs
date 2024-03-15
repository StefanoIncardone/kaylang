use crate::{
    cli::Utf8Path,
    error::ErrorInfo as ArtifactsErrorInfo,
    logging::{CAUSE, ERROR},
    src_file::SrcFile,
};
use std::{fmt::Display, io, path::PathBuf, process::Command};

#[derive(Debug)]
pub struct Assembler {
    pub(crate) path: Utf8Path,
}

impl Assembler {
    pub fn assemble(&self, linker: &Linker) -> Command {
        let asm_path_str = self.path.to_str().unwrap();
        let obj_path_str = linker.path.to_str().unwrap();
        let mut assembler_command = Command::new("nasm");
        let _ = assembler_command.args(["-felf64", "-gdwarf", &asm_path_str, "-o", &obj_path_str]);
        assembler_command
    }

    pub fn path(&self) -> &Utf8Path {
        &self.path
    }
}

#[derive(Debug)]
pub struct Linker {
    pub(crate) path: Utf8Path,
}

impl Linker {
    pub fn link(&self, runner: &Runner) -> Command {
        let obj_path_str = self.path.to_str().unwrap();
        let exe_path_str = runner.path.to_str().unwrap();
        let mut linker_command = Command::new("ld");
        let _ = linker_command.args([&obj_path_str, "-o", &exe_path_str]);
        linker_command
    }

    pub fn path(&self) -> &Utf8Path {
        &self.path
    }
}

#[derive(Debug)]
pub struct Runner {
    pub(crate) path: Utf8Path,
}

impl Runner {
    pub fn run(&self) -> Command {
        let exe_path_str = self.path.to_str().unwrap();
        Command::new(exe_path_str)
    }

    pub fn path(&self) -> &Utf8Path {
        &self.path
    }
}

#[derive(Debug)]
pub struct Artifacts {
    pub assembler: Assembler,
    pub linker: Linker,
    pub runner: Runner,
}

impl Artifacts {
    pub fn try_from_src(src: &SrcFile, out_path: Option<&Utf8Path>) -> Result<Self, Error> {
        // Safety: we know src_path is valid utf8 so can safely transmute
        let mut assembler: Assembler =
            unsafe { std::mem::transmute(src.path.with_extension("asm")) };
        let mut linker: Linker = unsafe { std::mem::transmute(src.path.with_extension("o")) };
        let mut runner: Runner = unsafe { std::mem::transmute(src.path.with_extension("")) };

        if let Some(out_path) = out_path {
            match std::fs::create_dir_all(&out_path.inner) {
                Ok(()) => {}
                Err(err) if err.kind() == io::ErrorKind::AlreadyExists => {}
                Err(err) => {
                    return Err(Error::CouldNotCreateOutputDirectory {
                        err,
                        path: out_path.inner.clone(),
                    });
                }
            }

            // TODO(stefano): ensure a file name is present instead of unwrapping
            assembler.path.inner = out_path.join(assembler.path.file_name().unwrap());
            linker.path.inner = out_path.join(linker.path.file_name().unwrap());
            runner.path.inner = out_path.join(runner.path.file_name().unwrap());
        }

        Ok(Self { assembler, linker, runner })
    }
}

#[derive(Debug)]
pub enum Error {
    CouldNotCreateOutputDirectory { err: io::Error, path: PathBuf },
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{info}", info = self.info())
    }
}

impl ArtifactsErrorInfo for Error {
    type Info = ErrorInfo;

    fn info(&self) -> Self::Info {
        let (msg, cause) = match self {
            Self::CouldNotCreateOutputDirectory { err, path } => (
                format!("could not create output directory '{path}'", path = path.display()),
                format!("{err} ({kind})", kind = err.kind()),
            ),
        };

        Self::Info { msg, cause }
    }
}

#[derive(Debug, Clone)]
pub struct ErrorInfo {
    pub msg: String,
    pub cause: String,
}

impl Display for ErrorInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{ERROR}: {msg}\
            \n{CAUSE}: {cause}",
            msg = self.msg,
            cause = self.cause
        )
    }
}
