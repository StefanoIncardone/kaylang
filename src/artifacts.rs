use crate::{
    cli::Utf8Path,
    error::ErrorInfo as ArtifactsErrorInfo,
    logging::{CAUSE, ERROR},
    src_file::SrcFile,
};
use std::{fmt::Display, io, path::PathBuf, process::Command};

#[derive(Debug)]
pub struct AsmPath {
    pub(crate) inner: Utf8Path,
}

impl AsmPath {
    pub fn inner(&self) -> &Utf8Path {
        &self.inner
    }
}

#[derive(Debug)]
pub struct ObjPath {
    pub(crate) inner: Utf8Path,
}

impl ObjPath {
    pub fn inner(&self) -> &Utf8Path {
        &self.inner
    }
}

#[derive(Debug)]
pub struct ExePath {
    pub(crate) inner: Utf8Path,
}

impl ExePath {
    pub fn inner(&self) -> &Utf8Path {
        &self.inner
    }
}

#[derive(Debug)]
pub struct Artifacts {
    pub asm_path: AsmPath,
    pub obj_path: ObjPath,
    pub exe_path: ExePath,
}

impl Artifacts {
    pub fn try_from_src(src: &SrcFile, out_path: Option<&Utf8Path>) -> Result<Self, Error> {
        // Safety: we know src_path is valid utf8 so can safely transmute
        let mut asm_path: AsmPath = unsafe { std::mem::transmute(src.path.with_extension("asm")) };
        let mut obj_path: ObjPath = unsafe { std::mem::transmute(src.path.with_extension("o")) };
        let mut exe_path: ExePath = unsafe { std::mem::transmute(src.path.with_extension("")) };

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
            asm_path.inner.inner = out_path.join(asm_path.inner.file_name().unwrap());
            obj_path.inner.inner = out_path.join(obj_path.inner.file_name().unwrap());
            exe_path.inner.inner = out_path.join(exe_path.inner.file_name().unwrap());
        }

        Ok(Self { asm_path, obj_path, exe_path })
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

#[derive(Clone, Copy, Debug)]
pub struct Assembler;

impl Assembler {
    pub fn assemble(asm_path: &AsmPath, obj_path: &ObjPath) -> Command {
        let asm_path_str = asm_path.inner.to_str().unwrap();
        let obj_path_str = obj_path.inner.to_str().unwrap();
        let mut assembler_command = Command::new("nasm");
        let _ = assembler_command.args(["-felf64", "-gdwarf", &asm_path_str, "-o", &obj_path_str]);
        assembler_command
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Linker;

impl Linker {
    pub fn link(obj_path: &ObjPath, exe_path: &ExePath) -> Command {
        let obj_path_str = obj_path.inner.to_str().unwrap();
        let exe_path_str = exe_path.inner.to_str().unwrap();
        let mut linker_command = Command::new("ld");
        let _ = linker_command.args([&obj_path_str, "-o", &exe_path_str]);
        linker_command
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Run;

impl Run {
    pub fn run(exe_path: &ExePath) -> Command {
        let exe_path_str = exe_path.inner.to_str().unwrap();
        Command::new(exe_path_str)
    }
}
