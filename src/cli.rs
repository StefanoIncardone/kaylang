use std::{env::Args, fmt::Display, path::PathBuf};

use crate::error::CliError;

#[derive(Debug, Default, Clone, Copy)]
pub enum Color {
    #[default]
    Auto,
    Always,
    Never,
}

impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Always => write!(f, "always"),
            Self::Auto => write!(f, "auto"),
            Self::Never => write!(f, "never"),
        };
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum Verbosity {
    #[default]
    Normal,
    Quiet,
    Verbose,
}

impl Display for Verbosity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Quiet => write!(f, "quiet"),
            Self::Normal => write!(f, "normal"),
            Self::Verbose => write!(f, "verbose"),
        };
    }
}

#[derive(Debug, Default, Clone)]
pub enum RunMode {
    #[default]
    Help,
    Version,
    Check {
        src_path: PathBuf,
    },
    Compile {
        src_path: PathBuf,
        out_path: Option<PathBuf>,
        run: bool,
    },
}

impl Display for RunMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Help => write!(f, "help"),
            Self::Version => write!(f, "version"),
            Self::Check { src_path } => write!(f, "check {}", src_path.display()),
            Self::Compile { src_path, out_path, run } => {
                if *run {
                    write!(f, "run ")?;
                } else {
                    write!(f, "compile ")?;
                }

                write!(f, "{} ", src_path.display())?;

                if let Some(path) = out_path {
                    write!(f, "-o {}", path.display())?
                }

                Ok(())
            }
        };
    }
}

#[derive(Debug, Default, Clone)]
pub struct KayArgs {
    pub color: Option<Color>,
    pub verbosity: Option<Verbosity>,
    pub run_mode: Option<RunMode>,
}

impl TryFrom<Vec<String>> for KayArgs {
    type Error = CliError;

    fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
        let args_iter = args.iter().peekable();

        let mut args = args_iter.clone();
        let _ = args.next(); // skipping the name of this executable

        Color::Auto.set();
        let mut color_mode: Option<Color> = None;

        while let Some(arg) = args.next() {
            if arg == "-c" || arg == "--color" {
                if let Some(mode) = color_mode {
                    return Err(CliError { msg: format!("'{}' color mode already selected", mode).into() });
                }

                let Some(mode) = args.next() else {
                    return Err(CliError { msg: "expected color mode".into() });
                };

                color_mode = match mode.as_str() {
                    "auto" => Some(Color::Auto),
                    "always" => Some(Color::Always),
                    "never" => Some(Color::Never),
                    _ => return Err(CliError { msg: "unrecognized color mode".into() }),
                };
            }
        }

        let color = color_mode.unwrap_or_default();
        color.set();

        let mut verbosity: Option<Verbosity> = None;
        let mut run_mode: Option<RunMode> = None;

        args = args_iter.clone();
        let _ = args.next(); // skipping the name of this executable

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-h" | "--help" => match run_mode {
                    Some(RunMode::Help) => {
                        return Err(CliError { msg: format!("'{}' help command already selected", arg).into() })
                    }
                    Some(RunMode::Version) => {
                        return Err(CliError { msg: "help and version commands cannot be used together".into() })
                    }
                    _ => run_mode = Some(RunMode::Help),
                },
                "-v" | "--version" => match run_mode {
                    Some(RunMode::Version) => {
                        return Err(CliError { msg: format!("'{}' version command already selected", arg).into() })
                    }
                    Some(RunMode::Help) => {
                        return Err(CliError { msg: "help and version commands cannot be used together".into() })
                    }
                    _ => run_mode = Some(RunMode::Version),
                },
                "check" | "compile" | "run" => {
                    if let Some(RunMode::Check { .. } | RunMode::Compile { .. }) = run_mode {
                        return Err(CliError { msg: format!("'{}' run mode already selected", arg).into() });
                    }

                    let src_path: PathBuf = match args.next() {
                        Some(path) => path.into(),
                        None => {
                            return Err(CliError { msg: format!("missing source file path for '{}' mode", arg).into() })
                        }
                    };

                    let mode = match arg.as_str() {
                        "check" => RunMode::Check { src_path },
                        "compile" | "run" => {
                            let mut out: Option<PathBuf> = None;

                            if let Some(out_flag) = args.peek() {
                                if *out_flag == "-o" || *out_flag == "--output" {
                                    let _ = args.next();

                                    out = match args.next() {
                                        Some(path) => Some(path.into()),
                                        None => return Err(CliError { msg: "missing output folder path".into() }),
                                    };
                                }
                            }

                            match arg.as_str() {
                                "compile" => RunMode::Compile { src_path, out_path: out, run: false },
                                "run" => RunMode::Compile { src_path, out_path: out, run: true },
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    };

                    if let Some(RunMode::Help | RunMode::Version) = run_mode {
                        // this is just to make sure that run modes commands are properly
                        // formatted, so we do nothing in the case where the -h, --help, -v
                        // or --version command was already selected
                    } else {
                        run_mode = Some(mode);
                    }
                }
                "-q" | "--quiet" | "-V" | "--verbose" => {
                    if let Some(mode) = verbosity {
                        return Err(CliError { msg: format!("'{}' verbosity mode already selected", mode).into() });
                    }

                    verbosity = match arg.as_str() {
                        "-q" | "--quiet" => Some(Verbosity::Quiet),
                        "-V" | "--verbose" => Some(Verbosity::Verbose),
                        _ => unreachable!(),
                    };
                }
                "-o" | "--output" => {
                    let Some(_) = args.next() else {
                        return Err(CliError { msg: "missing output folder path".into() });
                    };

                    return Err(CliError {
                        msg: "output folder option can only be used after a 'compile' or 'run' command".into(),
                    });
                }
                "-c" | "--color" => {
                    let _ = args.next();
                }
                _ => return Err(CliError { msg: format!("unrecognized option '{}'", arg).into() }),
            }
        }

        return Ok(Self { color: Some(color), verbosity, run_mode });
    }
}

impl TryFrom<Args> for KayArgs {
    type Error = CliError;

    fn try_from(args: Args) -> Result<Self, Self::Error> {
        return Self::try_from(args.collect::<Vec<String>>());
    }
}
