use crate::{Color, RunMode, Verbosity, ERROR};
use std::{
    fmt::Display,
    ops::Deref,
    path::{Path, PathBuf},
};

#[derive(Debug, Default, Clone)]
pub struct Args {
    pub color: Color,
    pub verbosity: Verbosity,
    pub run_mode: RunMode,
}

#[derive(Debug, Clone)]
pub struct FilePath {
    pub(crate) inner: PathBuf,
}

impl FilePath {
    pub fn from<P: AsRef<Path>>(path: P) -> Option<Self> {
        let path = path.as_ref().to_path_buf();
        if path.is_dir() {
            return None;
        }

        Some(Self { inner: path })
    }

    pub fn into_inner(self) -> PathBuf {
        self.inner
    }
}

impl Deref for FilePath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Debug, Clone)]
pub struct DirPath {
    pub(crate) inner: PathBuf,
}

impl DirPath {
    pub fn from<P: AsRef<Path>>(path: P) -> Option<Self> {
        let path = path.as_ref().to_path_buf();
        if path.is_file() {
            return None;
        }

        Some(Self { inner: path })
    }

    pub fn into_inner(self) -> PathBuf {
        self.inner
    }
}

impl Deref for DirPath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
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
                    _ => {
                        return Err(Error {
                            kind: ErrorKind::UnrecognizedColorMode { mode: mode.clone() },
                        });
                    }
                };
            }
        }

        let color = color.unwrap_or_default();
        color.set(&std::io::stderr());

        let mut verbosity: Option<Verbosity> = None;
        let mut command: Option<RunMode> = None;

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
                "-h" | "--help" => match command {
                    Some(RunMode::Help) => {
                        return Err(Error { kind: ErrorKind::HelpCommandAlreadySelected });
                    }
                    Some(RunMode::Version) => {
                        return Err(Error { kind: ErrorKind::HelpAndVersionCommandSelected });
                    }
                    _ => command = Some(RunMode::Help),
                },
                "-v" | "--version" => match command {
                    Some(RunMode::Version) => {
                        return Err(Error { kind: ErrorKind::VersionCommandAlreadySelected });
                    }
                    Some(RunMode::Help) => {
                        return Err(Error { kind: ErrorKind::HelpAndVersionCommandSelected });
                    }
                    _ => command = Some(RunMode::Version),
                },
                command_str @ ("check" | "compile" | "run") => {
                    if let Some(
                        RunMode::Check { .. } | RunMode::Compile { .. } | RunMode::Run { .. },
                    ) = command
                    {
                        return Err(Error {
                            kind: ErrorKind::CommandAlreadySelected {
                                command: command_str.to_string(),
                            },
                        });
                    }

                    let Some(path) = args.next() else {
                        return Err(Error {
                            kind: ErrorKind::MissingSourceFilePathForCommand {
                                command: command_str.to_string(),
                            },
                        });
                    };

                    let Some(src_path) = FilePath::from(path) else {
                        return Err(Error {
                            kind: ErrorKind::ExpectedFile { path: DirPath { inner: path.into() } },
                        });
                    };

                    let mode = match command_str {
                        "check" => RunMode::Check { src_path },
                        "compile" | "run" => {
                            let mut out_path = None;

                            if let Some(out_flag) = args.peek() {
                                if *out_flag == "-o" || *out_flag == "--output" {
                                    let _ = args.next();

                                    let Some(path) = args.next() else {
                                        return Err(Error {
                                            kind: ErrorKind::MissingOutputFolderPathForCommand {
                                                command: command_str.to_string(),
                                            },
                                        });
                                    };

                                    let Some(dir_path) = DirPath::from(path) else {
                                        return Err(Error {
                                            kind: ErrorKind::ExpectedDirectory {
                                                path: FilePath { inner: path.into() },
                                            },
                                        });
                                    };

                                    out_path = Some(dir_path);
                                }
                            }

                            match command_str {
                                "compile" => RunMode::Compile { src_path, out_path },
                                "run" => RunMode::Run { src_path, out_path },
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    };

                    if let Some(RunMode::Help | RunMode::Version) = command {
                        // this is just to make sure that run modes commands are properly formatted,
                        // so we do nothing in the case where the -h, --help, -v or --version command was already selected
                    } else {
                        command = Some(mode);
                    }
                }
                "-o" | "--output" => match args.next() {
                    Some(_) => return Err(Error { kind: ErrorKind::MissingOutputFolderPath }),
                    None => return Err(Error { kind: ErrorKind::StrayOutputFolderPath }),
                },
                "-c" | "--color" => {
                    let _ = args.next();
                }
                unrecognized => {
                    return Err(Error {
                        kind: ErrorKind::UnrecognizedFlag { flag: unrecognized.to_string() },
                    });
                }
            }
        }

        Ok(Self {
            color,
            verbosity: verbosity.unwrap_or_default(),
            run_mode: command.unwrap_or_default(),
        })
    }
}

impl TryFrom<std::env::Args> for Args {
    type Error = Error;

    fn try_from(args: std::env::Args) -> Result<Self, Self::Error> {
        Self::try_from(args.collect::<Vec<String>>())
    }
}

// TODO(stefano): add information about the command line arguments and pointers to the place where
// the error occured (akin to syntax errors)
#[derive(Debug, Clone)]
pub enum ErrorKind {
    ExpectedFile { path: DirPath },
    ExpectedDirectory { path: FilePath },
    ColorModeAlreadySelected,
    MissingColorMode,
    UnrecognizedColorMode { mode: String },

    VerbosityModeAlreadySelected,

    HelpCommandAlreadySelected,
    VersionCommandAlreadySelected,
    HelpAndVersionCommandSelected,

    CommandAlreadySelected { command: String },
    MissingSourceFilePathForCommand { command: String },
    MissingOutputFolderPathForCommand { command: String },
    MissingOutputFolderPath,
    StrayOutputFolderPath,

    UnrecognizedFlag { flag: String },
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedFile { path } => {
                write!(f, "expected a file but got directory '{path}'", path = path.display())
            }
            Self::ExpectedDirectory { path } => {
                write!(f, "expected a directory but got file '{path}'", path = path.display())
            }
            Self::ColorModeAlreadySelected => write!(f, "color mode selected more than once"),
            Self::MissingColorMode => write!(f, "missing color mode after"),
            Self::UnrecognizedColorMode { mode } => write!(f, "unrecognized color mode '{mode}'"),
            Self::VerbosityModeAlreadySelected => {
                write!(f, "verbosity mode selected more than once")
            }
            Self::HelpCommandAlreadySelected => write!(f, "help command selected more than once"),
            Self::VersionCommandAlreadySelected => {
                write!(f, "version command selected more than once")
            }
            Self::HelpAndVersionCommandSelected => {
                write!(f, "help and version commands cannot be selected at the same time")
            }
            Self::CommandAlreadySelected { command } => {
                write!(f, "'{command}' command already selected")
            }
            Self::MissingSourceFilePathForCommand { command } => {
                write!(f, "missing source file path for '{command}' command")
            }
            Self::MissingOutputFolderPathForCommand { command } => {
                write!(f, "missing output folder path for '{command}' command")
            }
            Self::MissingOutputFolderPath => write!(f, "missing output folder path"),
            Self::StrayOutputFolderPath => {
                write!(
                    f,
                    "output folder path option can only be used after a 'compile' or 'run' command"
                )
            }
            Self::UnrecognizedFlag { flag } => write!(f, "unrecognized flag '{flag}'"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{ERROR}: {msg}", msg = self.kind)
    }
}
