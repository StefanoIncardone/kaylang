use std::{path::{Path, PathBuf}, env::Args, fmt::Display, process::Command};

pub mod logging;
use logging::*;

mod lexer;
use lexer::*;

mod parser;
use parser::*;

mod compiler;
use compiler::*;


// Command line arguments
#[derive( Debug, Default, Clone, Copy )]
pub enum Color {
    #[default] Auto,
    Always,
    Never,
}

impl Display for Color {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Always => write!( f, "always" ),
            Self::Auto => write!( f, "auto" ),
            Self::Never => write!( f, "never" ),
        }
    }
}

#[derive( Debug, Default, Clone, Copy )]
pub enum Verbosity {
    #[default] Normal,
    Quiet,
    Verbose,
}

impl Display for Verbosity {
    fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
        return match self {
            Self::Quiet => write!( f, "quiet" ),
            Self::Normal => write!( f, "normal" ),
            Self::Verbose => write!( f, "verbose" ),
        }
    }
}

#[derive( Debug, Default, Clone )]
pub enum RunMode {
    #[default] Help,
    Version,
    Check { src_path: PathBuf },
    Compile { src_path: PathBuf, out_path: Option<PathBuf>, run: bool },
}

#[derive( Debug, Default, Clone )]
pub struct KayArgs {
    pub color: Option<Color>,
    pub verbosity: Option<Verbosity>,
    pub run_mode: Option<RunMode>,
}


pub struct Help {
    color: Color,
    full: bool,
}

impl Help {
    pub fn execute( &self ) {
        self.color.set_stdout();
        println!( "Kaylang compiler, version {}", VERSION );

        if self.full {
            println!( r"
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
}

pub enum CompileKind {
    Check,
    Compile { out_path: Option<PathBuf>, run: bool },
}

pub struct Compile {
    src: Src,
    verbosity: Verbosity,
    kind: CompileKind,
}

impl Compile {
    pub fn execute( &mut self ) -> Result<(), KayError>  {
        let mut logger = CompilationLogger::new( self.verbosity );
        logger.step( &CHECKING, &self.src.path );

        match self.src.populate() {
            Ok( src ) => src,
            Err( err ) => return Err( KayError::Src( err ) ),
        };

        let tokens_result = Lexer::tokenize( &self.src );
        logger.substep( &LEXING );
        let tokens = match tokens_result {
            Ok( tokens ) => tokens,
            Err( err ) => return Err( KayError::Syntax( err ) ),
        };

        let ast_result = Ast::build( &tokens, &self.src );
        logger.substep( &PARSING );
        let ast = match ast_result {
            Ok( ast ) => ast,
            Err( err ) => return Err( KayError::Syntax( err ) ),
        };

        logger.substep_done();
        match &self.kind {
            CompileKind::Check => logger.done(),
            CompileKind::Compile { out_path, run } => {
                let exe_path = match Compiler::compile( &self.src.path, out_path, &ast, &mut logger ) {
                    Ok( exe_path ) => exe_path,
                    Err( err ) => return Err( KayError::Compilation( err ) ),
                };

                logger.done();
                if !*run {
                    return Ok( () );
                }

                logger.step( &RUNNING, &exe_path );

                let mut executable = match Command::new( Path::new( "." ).join( &exe_path ) ).spawn() {
                    Ok( executable ) => executable,
                    Err( err ) => return Err( KayError::Running( IoError {
                        kind: err.kind(),
                        msg: format!( "could not create executable process '{}'", exe_path.display() ).into(),
                        cause: err.to_string().into(),
                    } ) ),
                };

                match executable.wait() {
                    Ok( _ ) => {},
                    Err( err ) => return Err( KayError::Running( IoError {
                        kind: err.kind(),
                        msg: format!( "could not run executable '{}'", exe_path.display() ).into(),
                        cause: err.to_string().into(),
                    } ) ),
                }
            }
        }

        return Ok( () );
    }
}


pub enum Kay {
    Help( Help ),
    Compile( Compile ),
}

impl From<KayArgs> for Kay {
    fn from( args: KayArgs ) -> Self {
        let color = args.color.unwrap_or_default();
        let verbosity = args.verbosity.unwrap_or_default();
        let run_mode = args.run_mode.unwrap_or_default();
        color.set();

        return match run_mode {
            RunMode::Help => Self::Help( Help { color, full: true } ),
            RunMode::Version => Self::Help( Help { color, full: false } ),
            RunMode::Check { src_path } => Self::Compile( Compile {
                src: Src { path: src_path, code: String::new(), lines: Vec::new() },
                verbosity,
                kind: CompileKind::Check,
            } ),
            RunMode::Compile { src_path, out_path, run } => Self::Compile( Compile {
                src: Src { path: src_path, code: String::new(), lines: Vec::new() },
                verbosity,
                kind: CompileKind::Compile { out_path, run },
            } ),
        }
    }
}

// Command line arguments parsing
impl TryFrom<Vec<String>> for Kay {
    type Error = CliError;

    fn try_from( args: Vec<String> ) -> Result<Self, Self::Error> {
        let args_iter = args.iter().peekable();

        let mut args = args_iter.clone();
        args.next(); // skipping the name of this executable

        Color::Auto.set();
        let mut color_mode: Option<Color> = None;

        while let Some( arg ) = args.next() {
            if arg == "-c" || arg == "--color" {
                if let Some( mode ) = color_mode {
                    return Err( CliError { msg: format!( "'{}' color mode already selected", mode ).into() } );
                }

                let Some( mode ) = args.next() else {
                    return Err( CliError { msg: "expected color mode".into() } );
                };

                color_mode = match mode.as_str() {
                    "auto" => Some( Color::Auto ),
                    "always" => Some( Color::Always ),
                    "never" => Some( Color::Never ),
                    _ => return Err( CliError { msg: "unrecognized color mode".into() } ),
                };
            }
        }

        let color = color_mode.unwrap_or_default();
        color.set();

        let mut verbosity: Option<Verbosity> = None;
        let mut run_mode: Option<RunMode> = None;

        args = args_iter.clone();
        args.next(); // skipping the name of this executable

        while let Some( arg ) = args.next() {
            match arg.as_str() {
                "-h" | "--help" => match run_mode {
                    Some( RunMode::Help ) => return Err(
                        CliError { msg: format!( "'{}' help command already selected", arg ).into() }
                    ),
                    Some( RunMode::Version ) => return Err(
                        CliError { msg: "help and version commands cannot be used together".into() }
                    ),
                    _ => run_mode = Some( RunMode::Help ),
                },
                "-v" | "--version" => match run_mode {
                    Some( RunMode::Version ) => return Err(
                        CliError { msg: format!( "'{}' version command already selected", arg ).into()
                    } ),
                    Some( RunMode::Help ) => return Err(
                        CliError { msg: "help and version commands cannot be used together".into() }
                    ),
                    _ => run_mode = Some( RunMode::Version ),
                },
                "check"| "compile" | "run" => {
                    if let Some( RunMode::Check { .. } | RunMode::Compile { .. } ) = run_mode {
                        return Err( CliError { msg: format!( "'{}' run mode already selected", arg ).into() } );
                    }

                    let src_path: PathBuf = match args.next() {
                        Some( path ) => path.into(),
                        None => return Err( CliError { msg: format!( "missing source file path for '{}' mode", arg ).into() } ),
                    };

                    let mode = match arg.as_str() {
                        "check" => RunMode::Check { src_path },
                        "compile" | "run" => {
                            let mut out: Option<PathBuf> = None;

                            if let Some( out_flag ) = args.peek() {
                                if *out_flag == "-o" || *out_flag == "--output" {
                                    args.next();

                                    out = match args.next() {
                                        Some( path ) => Some( path.into() ),
                                        None => return Err( CliError { msg: "missing output folder path".into() } ),
                                    };
                                }
                            }

                            match arg.as_str() {
                                "compile" => RunMode::Compile { src_path, out_path: out, run: false },
                                "run"     => RunMode::Compile { src_path, out_path: out, run: true },
                                _         => unreachable!(),
                            }
                        },
                        _ => unreachable!(),
                    };

                    if let Some( RunMode::Help | RunMode::Version ) = run_mode {
                        // this is just to make sure that run modes commands are properly
                        // formatted, so we do nothing in the case where the -h, --help, -v
                        // or --version command was already selected
                    }
                    else {
                        run_mode = Some( mode );
                    }
                },
                "-q" | "--quiet" | "-V" | "--verbose" => {
                    if let Some( mode ) = verbosity {
                        return Err( CliError { msg: format!( "'{}' verbosity mode already selected", mode ).into() } );
                    }

                    verbosity = match arg.as_str() {
                        "-q" | "--quiet"   => Some( Verbosity::Quiet ),
                        "-V" | "--verbose" => Some( Verbosity::Verbose ),
                        _                  => unreachable!(),
                    };
                },
                "-o" | "--output" => {
                    let Some( _ ) = args.next() else {
                        return Err( CliError { msg: "missing output folder path".into() } );
                    };

                    return Err( CliError { msg: "output folder option can only be used after a 'compile' or 'run' command".into() } );
                },
                "-c" | "--color" => { args.next(); },
                _ => return Err( CliError { msg: format!( "unrecognized option '{}'", arg ).into() } ),
            }
        }

        return Ok( Self::from( KayArgs { color: Some( color ), verbosity, run_mode } ) );
    }
}

impl TryFrom<Args> for Kay {
    type Error = CliError;

    fn try_from( args: Args ) -> Result<Self, Self::Error> {
        return Self::try_from( args.collect::<Vec<String>>() );
    }
}

// factory methods
impl Kay {
    pub fn help( color: Color ) -> Help {
        return Help { color, full: true };
    }

    pub fn version( color: Color ) -> Help {
        return Help { color, full: false };
    }

    pub fn check( path: impl Into<PathBuf>, verbosity: Verbosity ) -> Compile {
        return Compile {
            src: Src { path: path.into(), code: String::new(), lines: Vec::new() },
            verbosity,
            kind: CompileKind::Check
        };
    }

    pub fn compile( path: impl Into<PathBuf>, verbosity: Verbosity, out_path: Option<PathBuf>, run: bool ) -> Compile {
        return Compile {
            src: Src { path: path.into(), code: String::new(), lines: Vec::new() },
            verbosity,
            kind: CompileKind::Compile { out_path, run }
        };
    }
}

impl Kay {
    pub fn execute( &mut self ) -> Result<(), KayError> {
        return match self {
            Self::Help( help ) => {
                help.execute();
                Ok( () )
            },
            Self::Compile( compile ) => compile.execute(),
        }
    }
}
