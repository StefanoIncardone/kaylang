// TODO(stefano): rename typ to type_ or something more consisten with other variable names

use std::{
    env::Args,
    path::{Path, PathBuf},
    process::Command,
};

use ast::Ast;
pub use cli::{Color, KayArgs, RunMode, Verbosity};
use compiler::Compiler;
use error::{CliError, IoError, KayError};
use lexer::{Lexer, SrcFile};
use logging::{
    CompilationLogger, AST_BUILDING, CHECKING, FILE, LEXING, MODE, OPTIONS, OUTPUT, PATH, RUNNING, RUN_MODE, VERSION,
};

mod ast;
mod cli;
mod compiler;
mod error;
mod lexer;
mod logging;

#[derive(Clone, Copy, Debug)]
pub struct Version;

impl Version {
    pub fn print(color: Color) {
        color.set_stdout();
        println!("Kaylang compiler, version {}", VERSION);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Help;

impl Help {
    pub fn print(color: Color) {
        Version::print(color);

        println!(
            r"
Usage: kay [{OPTIONS}] [{RUN_MODE}]

{OPTIONS}:
-h, --help            Display this message
-v, --version         Display the compiler version
-c, --color <{MODE}>    Wether to display colored output ({MODE}: auto (default), never, always)
-q, --quiet           Don't display any diagnostic messages
-V, --verbose         Display extra diagnostic messages

{RUN_MODE}:
check    <{FILE}>              Check the source code for correctness
compile  <{FILE}> [{OUTPUT}]     Compile the source code down to an executable
run      <{FILE}> [{OUTPUT}]     Compile and run the generated executable

{OUTPUT}:
-o, --output <{PATH}>       Folder to populate with compilation artifacts (.asm, .o, executable) (default: '.')"
        );
    }
}

#[derive(Debug)]
pub enum CompileKind {
    Check,
    Compile { out_path: Option<PathBuf>, run: bool },
}

#[derive(Debug)]
pub struct Compile {
    pub src: SrcFile,
    pub verbosity: Verbosity,
    pub kind: CompileKind,
}

impl Compile {
    pub fn compile(&mut self) -> Result<(), KayError<'_>> {
        let mut logger = CompilationLogger::new(self.verbosity);
        logger.step(&CHECKING, &self.src.path);

        match self.src.parse() {
            Ok(src) => src,
            Err(err) => return Err(KayError::Src(err)),
        };

        let tokens_result = Lexer::tokenize(&self.src);
        logger.substep(&LEXING);
        let tokens = match tokens_result {
            Ok(tokens) => tokens,
            Err(err) => return Err(KayError::Syntax(err)),
        };

        let ast_result = Ast::build(&self.src, &tokens);
        logger.substep(&AST_BUILDING);
        let ast = match ast_result {
            Ok(ast) => ast,
            Err(err) => return Err(KayError::Syntax(err)),
        };

        logger.substep_done();
        match &self.kind {
            CompileKind::Check => logger.done(),
            CompileKind::Compile { out_path, run } => {
                let exe_path = match Compiler::compile(&self.src, out_path, &ast, &mut logger) {
                    Ok(exe_path) => exe_path,
                    Err(err) => return Err(KayError::Compilation(err)),
                };

                logger.done();
                if !*run {
                    return Ok(());
                }

                logger.step(&RUNNING, &exe_path);

                let mut executable = match Command::new(Path::new(".").join(&exe_path)).spawn() {
                    Ok(executable) => executable,
                    Err(err) => {
                        return Err(KayError::Running(IoError {
                            kind: err.kind(),
                            msg: format!("could not create executable process '{}'", exe_path.display()).into(),
                            cause: err.to_string().into(),
                        }))
                    }
                };

                match executable.wait() {
                    Ok(_) => {}
                    Err(err) => {
                        return Err(KayError::Running(IoError {
                            kind: err.kind(),
                            msg: format!("could not run executable '{}'", exe_path.display()).into(),
                            cause: err.to_string().into(),
                        }))
                    }
                }
            }
        }

        return Ok(());
    }
}

#[derive(Debug)]
pub enum Kay {
    Version(Color),
    Help(Color),
    // TODO(stefano): split into Compile and Run and make Run return the exitcode of the code it just run
    Compile(Compile),
}

impl TryFrom<Vec<String>> for Kay {
    type Error = CliError;

    fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
        let args = KayArgs::try_from(args)?;
        return Ok(Kay::from(args));
    }
}

impl TryFrom<Args> for Kay {
    type Error = CliError;

    fn try_from(args: Args) -> Result<Self, Self::Error> {
        return Self::try_from(args.collect::<Vec<String>>());
    }
}

impl From<KayArgs> for Kay {
    fn from(args: KayArgs) -> Self {
        let color = args.color.unwrap_or_default();
        let verbosity = args.verbosity.unwrap_or_default();
        let run_mode = args.run_mode.unwrap_or_default();
        color.set();

        return match run_mode {
            RunMode::Version => Self::Version(color),
            RunMode::Help => Self::Help(color),
            RunMode::Check { src_path } => {
                Self::Compile(Compile { src: src_path.into(), verbosity, kind: CompileKind::Check })
            }
            RunMode::Compile { src_path, out_path, run } => {
                Self::Compile(Compile { src: src_path.into(), verbosity, kind: CompileKind::Compile { out_path, run } })
            }
        };
    }
}

impl Kay {
    pub fn execute(&mut self) -> Result<(), KayError<'_>> {
        return match self {
            Self::Version(color) => {
                Version::print(*color);
                Ok(())
            }
            Self::Help(color) => {
                Help::print(*color);
                Ok(())
            }
            Self::Compile(compile) => compile.compile(),
        };
    }
}
