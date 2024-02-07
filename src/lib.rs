// TODO(stefano): rename typ to type_ or something more consisten with other variable names

use std::{
    env::Args,
    path::{Path, PathBuf},
    process::Command,
    time::Instant,
};

use ast::Ast;
use cli::{set_stderr, set_stdout};
pub use cli::{Color, KayArgs, RunMode, Verbosity};
use compiler::{Assembler, Compiler, Linker};
use error::{CliError, IoError, KayError};
use lexer::{Lexer, SrcFile};
use logging::{
    Step, SubStep, ASM_GENERATION, ASSEMBLER, AST_BUILDING, CHECKING, COMPILING, FILE, LEXING, LINKER, LOADING_SOURCE,
    MODE, OPTIONS, OUTPUT, PATH, RUNNING, RUN_MODE, SUBSTEP_DONE, VERSION,
};

mod ast;
mod cli;
mod color;
mod compiler;
mod error;
mod lexer;
mod logging;

#[derive(Clone, Copy, Debug)]
pub struct Version;

impl Version {
    pub fn print(color: Color) {
        set_stdout(color);
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
        let compilation_step = Step { start_time: Instant::now(), verbosity: self.verbosity };

        logging::info_step(&CHECKING, &self.src.path, self.verbosity);
        let checking_sub_step = SubStep { step: &SUBSTEP_DONE, start_time: Instant::now(), verbosity: self.verbosity };

        {
            let loading_source_sub_step =
                SubStep { step: &LOADING_SOURCE, start_time: Instant::now(), verbosity: self.verbosity };
            let source_loading_result = self.src.load();
            loading_source_sub_step.done();
            match source_loading_result {
                Ok(src) => src,
                Err(err) => return Err(KayError::Src(err)),
            };
        }

        let tokens = {
            let lexing_sub_step = SubStep { step: &LEXING, start_time: Instant::now(), verbosity: self.verbosity };
            let lexer_result = Lexer::tokenize(&self.src);
            lexing_sub_step.done();
            match lexer_result {
                Ok(tokens) => tokens,
                Err(err) => return Err(KayError::Syntax(err)),
            }
        };

        let ast = {
            let ast_building_sub_step =
                SubStep { step: &AST_BUILDING, start_time: Instant::now(), verbosity: self.verbosity };
            let ast_building_result = Ast::build(&self.src, &tokens);
            ast_building_sub_step.done();
            match ast_building_result {
                Ok(ast) => ast,
                Err(err) => return Err(KayError::Syntax(err)),
            }
        };

        checking_sub_step.done();

        match &self.kind {
            CompileKind::Check => compilation_step.done(),
            CompileKind::Compile { out_path, run } => {
                logging::info_step(&COMPILING, &self.src.path, self.verbosity);
                let compilation_sub_step =
                    SubStep { step: &SUBSTEP_DONE, start_time: Instant::now(), verbosity: self.verbosity };

                let (asm_path, obj_path, exe_path) = {
                    let asm_generation_sub_step =
                        SubStep { step: &ASM_GENERATION, start_time: Instant::now(), verbosity: self.verbosity };
                    let asm_generation_result = Compiler::compile(&self.src, out_path, &ast);
                    asm_generation_sub_step.done();
                    match asm_generation_result {
                        Ok(artifacts_path) => artifacts_path,
                        Err(err) => return Err(KayError::Compilation(err)),
                    }
                };

                {
                    let assembler_sub_step =
                        SubStep { step: &ASSEMBLER, start_time: Instant::now(), verbosity: self.verbosity };
                    let assembler_result = Assembler::assemble(&asm_path, &obj_path);
                    assembler_sub_step.done();
                    match assembler_result {
                        Ok(()) => {}
                        Err(err) => return Err(KayError::Compilation(err)),
                    }
                }

                {
                    let linker_sub_step =
                        SubStep { step: &LINKER, start_time: Instant::now(), verbosity: self.verbosity };
                    let linker_result = Linker::link(&obj_path, &exe_path);
                    linker_sub_step.done();
                    match linker_result {
                        Ok(()) => {}
                        Err(err) => return Err(KayError::Compilation(err)),
                    }
                }

                compilation_sub_step.done();

                compilation_step.done();

                if *run {
                    logging::info_step(&RUNNING, &exe_path, self.verbosity);

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
        set_stderr(color);

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
