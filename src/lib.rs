#![deny( private_bounds, private_interfaces )]
#![allow( clippy::needless_return )]

use std::{path::{PathBuf, Path}, env::Args, fmt::Display};

mod parser;
mod color;

pub mod errors;
use errors::*;

mod logging;
use logging::*;

mod lexer;
use lexer::*;

mod checker;
use checker::*;

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
    Compile { src_path: PathBuf, out: Option<PathBuf>, run: bool },
}

#[derive( Debug, Default, Clone )]
pub struct KayArgs {
    pub color: Option<Color>,
    pub verbosity: Option<Verbosity>,
    pub run_mode: Option<RunMode>,
}


pub struct Kay {
    color: Color,
    verbosity: Verbosity,
    run_mode: RunMode,
}

impl From<KayArgs> for Kay {
    fn from( args: KayArgs ) -> Self {
        let color = args.color.unwrap_or_default();
        let verbosity = args.verbosity.unwrap_or_default();
        let run_mode = args.run_mode.unwrap_or_default();

        color.set();
        return Self { color, verbosity, run_mode };
    }
}

impl TryFrom<Args> for Kay {
    type Error = CliError;

    fn try_from( args: Args ) -> Result<Self, Self::Error> {
        return Self::try_from( args.collect::<Vec<String>>() );
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
                                "compile" => RunMode::Compile { src_path, out, run: false },
                                "run"     => RunMode::Compile { src_path, out, run: true },
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

        let run_mode = run_mode.unwrap_or_default();
        let verbosity = verbosity.unwrap_or_default();
        return Ok( Self { color, verbosity, run_mode } );
    }
}


// Execution of specified commands
impl Kay {
    fn print_usage( &self ) {
        self.print_version();

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

    fn print_version( &self ) {
        self.color.set_stdout();
        println!( "Kaylang compiler, version {}", VERSION );
    }


    // IDEA remove KayError enum
        // TODO split into different compilation phases:
        // 0) help message (if present) (which does not return anything)
        // 1) source file reading       (which returns IoError)
        // 2) fron-end                  (which returns Vec<SyntaxError>)
        // 3) back-end                  (which returns IoError )
    pub fn execute( &self ) -> Result<(), KayError> {
        return match &self.run_mode {
            RunMode::Help => {
                self.print_usage();
                return Ok( () );
            },
            RunMode::Version => {
                self.print_version();
                return Ok( () );
            },
            RunMode::Check { src_path: src } | RunMode::Compile { src_path: src, .. } => {
                let mut logger = CompilationLogger::new( self.verbosity );

                let source_file = match Src::try_from( Path::new( &src ) ) {
                    Ok( src ) => src,
                    Err( err ) => return Err( KayError::Src( err ) ),
                };

                let scopes = match Checker::check( &source_file, &mut logger ) {
                    Ok( scopes ) => {
                        // println!( "{:#?}", ast );
                        scopes
                    },
                    Err( errors ) => return Err( KayError::Syntax( SyntaxErrors::new( errors, &source_file ) ) ),
                };

                match &self.run_mode {
                    RunMode::Check { .. } => logger.done(),
                    RunMode::Compile { src_path: src, out, run } => {
                        let mut compiler = Compiler {
                            src_path: src.clone(),
                            out_path: out.clone(),
                            run: *run,
                            scopes: &scopes,
                            rodata: String::new(),
                            asm: String::new(),
                            variables: Vec::new(),
                            strings: Vec::new(),
                            if_idx: 0,
                            loop_idx: 0,
                            loop_idx_stack: Vec::new(),
                        };

                        match compiler.compile( &mut logger ) {
                            Ok( _ ) => {},
                            Err( err ) => return Err( KayError::BackEnd( err ) ),
                        }
                    },
                    RunMode::Help | RunMode::Version => unreachable!(),
                }

                Ok( () )
            },
        }
    }
}