use crate::{
    error::{CliError, CliErrorInfo, ErrorInfo},
    Color, RunMode, Verbosity,
};
use std::path::PathBuf;

#[derive(Debug, Default, Clone)]
pub struct Args {
    pub color: Color,
    pub verbosity: Verbosity,
    pub run_mode: RunMode,
}

impl TryFrom<Vec<String>> for Args {
    type Error = Error;

    fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
        let args_iter = args.iter();

        let mut args = args_iter.clone();
        let _ = args.next(); // skipping the name of this executable

        Color::Auto.set(&std::io::stderr());
        let mut color: Option<Color> = None;

        while let Some(arg) = args.next() {
            if arg == "-c" || arg == "--color" {
                if let Some(_mode) = color {
                    return Err(Error { kind: ErrorKind::ColorModeAlreadySelected });
                }

                let Some(mode) = args.next() else {
                    return Err(Error { kind: ErrorKind::MissingColorMode });
                };

                color = match mode.as_str() {
                    "auto" => Some(Color::Auto),
                    "always" => Some(Color::Always),
                    "never" => Some(Color::Never),
                    _ => return Err(Error { kind: ErrorKind::UnrecognizedColorMode { unrecognized: mode.clone() } }),
                };
            }
        }

        let color = color.unwrap_or_default();
        color.set(&std::io::stderr());

        let mut verbosity: Option<Verbosity> = None;
        let mut run_mode: Option<RunMode> = None;

        let mut args = args_iter.clone().peekable();
        let _ = args.next(); // skipping the name of this executable

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-q" | "--quiet" | "-V" | "--verbose" => {
                    if let Some(_mode) = verbosity {
                        return Err(Error { kind: ErrorKind::VerbosityModeAlreadySelected });
                    }

                    verbosity = match arg.as_str() {
                        "-q" | "--quiet" => Some(Verbosity::Quiet),
                        "-V" | "--verbose" => Some(Verbosity::Verbose),
                        _ => unreachable!(),
                    };
                }
                "-h" | "--help" => match run_mode {
                    Some(RunMode::Help) => return Err(Error { kind: ErrorKind::HelpCommandAlreadySelected }),
                    Some(RunMode::Version) => return Err(Error { kind: ErrorKind::HelpAndVersionCommandSelected }),
                    _ => run_mode = Some(RunMode::Help),
                },
                "-v" | "--version" => match run_mode {
                    Some(RunMode::Version) => return Err(Error { kind: ErrorKind::VersionCommandAlreadySelected }),
                    Some(RunMode::Help) => return Err(Error { kind: ErrorKind::HelpAndVersionCommandSelected }),
                    _ => run_mode = Some(RunMode::Version),
                },
                run_mode_str @ ("check" | "compile" | "run") => {
                    if let Some(RunMode::Check { .. } | RunMode::Compile { .. } | RunMode::Run { .. }) = run_mode {
                        return Err(Error {
                            kind: ErrorKind::RunModeAlreadySelected { mode: run_mode_str.to_string() },
                        });
                    }

                    let src_path: PathBuf = match args.next() {
                        Some(path) => path.into(),
                        None => {
                            return Err(Error {
                                kind: ErrorKind::MissingSourceFilePathForRunMode { mode: run_mode_str.to_string() },
                            })
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
                                        None => {
                                            return Err(Error {
                                                kind: ErrorKind::MissingOutputFolderPathForRunMode {
                                                    mode: run_mode_str.to_string(),
                                                },
                                            })
                                        }
                                    };
                                }
                            }

                            match arg.as_str() {
                                "compile" => RunMode::Compile { src_path, out_path: out },
                                "run" => RunMode::Run { src_path, out_path: out },
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    };

                    if let Some(RunMode::Help | RunMode::Version) = run_mode {
                        // this is just to make sure that run modes commands are properly formatted,
                        // so we do nothing in the case where the -h, --help, -v or --version command was already selected
                    } else {
                        run_mode = Some(mode);
                    }
                }
                "-o" | "--output" => {
                    let Some(_) = args.next() else {
                        return Err(Error { kind: ErrorKind::MissingOutputFolderPath });
                    };

                    return Err(Error { kind: ErrorKind::StrayOutputFolderPath });
                }
                "-c" | "--color" => {
                    let _ = args.next();
                }
                unrecognized => {
                    return Err(Error { kind: ErrorKind::UnrecognizedFlag { flag: unrecognized.to_string() } })
                }
            }
        }

        Ok(Self { color, verbosity: verbosity.unwrap_or_default(), run_mode: run_mode.unwrap_or_default() })
    }
}

impl TryFrom<std::env::Args> for Args {
    type Error = Error;

    fn try_from(args: std::env::Args) -> Result<Self, Self::Error> {
        Self::try_from(args.collect::<Vec<String>>())
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    ColorModeAlreadySelected,
    MissingColorMode,
    UnrecognizedColorMode { unrecognized: String },

    VerbosityModeAlreadySelected,

    HelpCommandAlreadySelected,
    VersionCommandAlreadySelected,
    HelpAndVersionCommandSelected,

    RunModeAlreadySelected { mode: String },
    MissingSourceFilePathForRunMode { mode: String },
    MissingOutputFolderPathForRunMode { mode: String },
    MissingOutputFolderPath,
    StrayOutputFolderPath,

    UnrecognizedFlag { flag: String },
}

impl ErrorInfo for ErrorKind {
    type Info = CliErrorInfo;

    fn info(&self) -> Self::Info {
        let msg = match self {
            Self::ColorModeAlreadySelected => "color mode selected more than once".into(),
            Self::MissingColorMode => "missing color mode after".into(),
            Self::UnrecognizedColorMode { unrecognized } => format!("unrecognized color mode '{unrecognized}'").into(),

            Self::VerbosityModeAlreadySelected => "verbosity mode selected more than once".into(),

            Self::HelpCommandAlreadySelected => "help command selected more than once".into(),
            Self::VersionCommandAlreadySelected => "version command selected more than once".into(),
            Self::HelpAndVersionCommandSelected => {
                "help and version commands cannot be selected at the same time".into()
            }

            Self::RunModeAlreadySelected { mode } => format!("'{mode}' run mode already selected").into(),
            Self::MissingSourceFilePathForRunMode { mode } => {
                format!("missing source file path for '{mode}' mode").into()
            }
            Self::MissingOutputFolderPathForRunMode { mode } => {
                format!("missing output folder path for '{mode}' mode").into()
            }
            Self::MissingOutputFolderPath => "missing output folder path".into(),
            Self::StrayOutputFolderPath => {
                "output folder path option can only be used after a 'compile' or 'run' command".into()
            }

            Self::UnrecognizedFlag { flag } => format!("unrecognized flag '{flag}'").into(),
        };

        Self::Info { msg }
    }
}

pub type Error = CliError<ErrorKind>;
