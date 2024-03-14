pub mod ast;
pub mod cli;
pub mod compiler;
pub mod error;
pub mod logging;
pub mod src_file;
pub mod tokenizer;

use cli::Utf8Path;
use compiler::ErrorKind;
use error::BackEndError;
use logging::{Bg, Colored, Fg, Flag, Flags};
use src_file::SrcFile;
use std::{fmt::Display, io, process::Command};

#[derive(Debug, Default, Clone, Copy)]
pub enum Color {
    #[default]
    Auto,
    Always,
    Never,
}

#[derive(Debug, Default, Clone, Copy)]
pub enum Verbosity {
    #[default]
    Normal,
    Quiet,
    Verbose,
}

#[derive(Debug, Default, Clone)]
pub enum RunMode {
    #[default]
    Help,
    Version,
    Check {
        src_path: Utf8Path,
    },
    Compile {
        src_path: Utf8Path,
        out_path: Option<Utf8Path>,
    },
    Run {
        src_path: Utf8Path,
        out_path: Option<Utf8Path>,
    },
}

// help and version messages
const HELP_FG: Fg = Fg::White;
const HELP_BG: Bg = Bg::Default;
const HELP_FLAGS: Flags = Flag::Bold;

#[rustfmt::skip] pub(crate) static VERSION:  Colored<&str> = Colored { text: env!("CARGO_PKG_VERSION"), fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static OPTIONS:  Colored<&str> = Colored { text: "Options",                 fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static RUN_MODE: Colored<&str> = Colored { text: "Run mode",                fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static MODE:     Colored<&str> = Colored { text: "mode",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static FILE:     Colored<&str> = Colored { text: "file",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static PATH:     Colored<&str> = Colored { text: "path",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static OUTPUT:   Colored<&str> = Colored { text: "Output",                  fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };

#[derive(Clone, Copy, Debug)]
pub struct Version {
    pub color: Color,
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.color.set(&std::io::stdout());
        write!(f, "Kaylang compiler, version {VERSION}")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Help {
    pub color: Color,
}

impl Display for Help {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r"{version}

Usage: kay [{OPTIONS}] [{RUN_MODE}]

{OPTIONS}:
-h, --help            Display this message (selected when no other run commands are provided)
-v, --version         Display the compiler version
-c, --color <{MODE}>    Wether to display colored output ({MODE}: auto (default), never, always)
-q, --quiet           Don't display any diagnostic messages
-V, --verbose         Display extra diagnostic messages

{RUN_MODE}:
check    <{FILE}>              Check the source code for correctness
compile  <{FILE}> [{OUTPUT}]     Compile the source code down to an executable
run      <{FILE}> [{OUTPUT}]     Compile and run the generated executable

{OUTPUT}:
-o, --output <{PATH}>       Folder to populate with compilation artifacts (.asm, .o, executable) (default: '.')",
            version = Version { color: self.color }
        )
    }
}

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
    pub fn try_from_src(
        src: &SrcFile,
        out_path: Option<&Utf8Path>,
    ) -> Result<Self, BackEndError<ErrorKind>> {
        // Safety: we know src_path is valid utf8 so can safely transmute
        let mut asm_path: AsmPath = unsafe { std::mem::transmute(src.path.with_extension("asm")) };
        let mut obj_path: ObjPath = unsafe { std::mem::transmute(src.path.with_extension("o")) };
        let mut exe_path: ExePath = unsafe { std::mem::transmute(src.path.with_extension("")) };

        if let Some(out_path) = out_path {
            match std::fs::create_dir_all(&out_path.inner) {
                Ok(()) => {}
                Err(err) if err.kind() == io::ErrorKind::AlreadyExists => {}
                Err(err) => {
                    return Err(BackEndError {
                        kind: ErrorKind::CouldNotCreateOutputDirectory {
                            err,
                            path: out_path.inner.clone(),
                        },
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
