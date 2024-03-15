use crate::{
    cli::Utf8Path,
    error::ErrorInfo as ArtifactsErrorInfo,
    logging::{CAUSE, ERROR},
    src_file::SrcFile,
};
use std::{ffi::OsStr, fmt::Display, io, path::PathBuf, process::Command};

#[derive(Debug)]
pub struct Artifacts {
    pub asm_path: Utf8Path,
    pub obj_path: Utf8Path,
    pub exe_path: Utf8Path,
}

impl Artifacts {
    pub fn new(src: &SrcFile, out_path: Option<&Utf8Path>) -> Result<Self, Error> {
        // Safety: we know src_path is valid utf8 so can safely transmute
        let mut asm_path: Utf8Path = unsafe { std::mem::transmute(src.path.with_extension("asm")) };
        let mut obj_path: Utf8Path = unsafe { std::mem::transmute(src.path.with_extension("o")) };
        let mut exe_path: Utf8Path = unsafe { std::mem::transmute(src.path.with_extension("")) };

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
            asm_path.inner = out_path.join(asm_path.file_name().unwrap());
            obj_path.inner = out_path.join(obj_path.file_name().unwrap());
            exe_path.inner = out_path.join(exe_path.file_name().unwrap());
        }

        Ok(Self { asm_path, obj_path, exe_path })
    }

    pub fn assembler(&self) -> Command {
        let mut assembler_command = Command::new("nasm");
        let _ = assembler_command
            .arg(OsStr::new("-felf64"))
            .arg(OsStr::new("-gdwarf"))
            .arg(self.asm_path.as_os_str())
            .arg(OsStr::new("-o"))
            .arg(self.obj_path.as_os_str());
        assembler_command
    }

    pub fn linker(&self) -> Command {
        let mut linker_command = Command::new("ld");
        let _ = linker_command
            .arg(self.obj_path.as_os_str())
            .arg(OsStr::new("-o"))
            .arg(self.exe_path.as_os_str());
        linker_command
    }

    pub fn runner(&self) -> Command {
        Command::new(self.exe_path.as_os_str())
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
