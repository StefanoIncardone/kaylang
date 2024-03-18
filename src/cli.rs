use crate::{Bg, Color, Colored, Command, Fg, Flag, Verbosity, BAR, ERROR};
use std::{
    fmt::Display,
    ops::Deref,
    path::{Path, PathBuf},
};

#[derive(Debug, Default, Clone)]
pub struct Args {
    pub color: Color,
    pub verbosity: Verbosity,
    pub command: Command,
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

    #[must_use]
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

    #[must_use]
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
        let mut color_args = args.iter().enumerate().clone();
        let _ = color_args.next(); // skipping the name of this executable

        Color::Auto.set(&std::io::stderr());
        let mut color: Option<(Color, CliOption)> = None;

        while let Some((color_flag_idx, arg)) = color_args.next() {
            if arg == "-c" || arg == "--color" {
                let cli_option = match arg.as_str() {
                    "-c" => CliOption::ColorShort,
                    "--color" => CliOption::ColorLong,
                    _ => unreachable!(),
                };

                if let Some((_mode, previous_cli_option)) = color {
                    return Err(Error {
                        args,
                        erroneous_arg_index: color_flag_idx,
                        kind: ErrorKind::RepeatedOption(cli_option),
                        cause: ErrorCause::OptionAlreadySelected {
                            current: cli_option,
                            previous: previous_cli_option,
                        },
                    });
                }

                let Some((color_mode_idx, mode)) = color_args.next() else {
                    return Err(Error {
                        args,
                        erroneous_arg_index: color_flag_idx,
                        kind: ErrorKind::MissingColorMode,
                        cause: ErrorCause::MustBeFollowedByColorMode,
                    });
                };

                let color_mode = match mode.as_str() {
                    "auto" => Color::Auto,
                    "always" => Color::Always,
                    "never" => Color::Never,
                    _ => {
                        let mode = mode.to_owned();
                        return Err(Error {
                            args,
                            erroneous_arg_index: color_mode_idx,
                            kind: ErrorKind::UnrecognizedColorMode { mode },
                            cause: ErrorCause::UnrecognizedColorMode,
                        });
                    }
                };

                color = Some((color_mode, cli_option));
            }
        }

        let color = match color {
            Some((mode, _)) => mode,
            None => Color::Auto,
        };
        color.set(&std::io::stderr());

        let mut verbosity: Option<(Verbosity, CliOption)> = None;
        let mut command: Option<(Command, CliCommand)> = None;

        let mut other_args = args.iter().enumerate().clone().peekable();
        let _ = other_args.next(); // skipping the name of this executable

        while let Some((flag_idx, arg)) = other_args.next() {
            match arg.as_str() {
                "-q" | "--quiet" | "-V" | "--verbose" => {
                    let cli_option = match arg.as_str() {
                        "-q" => CliOption::QuietShort,
                        "--quiet" => CliOption::QuietLong,
                        "-V" => CliOption::VerboseLong,
                        "--verbose" => CliOption::VerboseLong,
                        _ => unreachable!(),
                    };

                    if let Some((_mode, previous_cli_option)) = verbosity {
                        return Err(Error {
                            args,
                            erroneous_arg_index: flag_idx,
                            kind: ErrorKind::RepeatedOption(cli_option),
                            cause: ErrorCause::OptionAlreadySelected {
                                current: cli_option,
                                previous: previous_cli_option,
                            },
                        });
                    }

                    let verbosity_mode = match arg.as_str() {
                        "-q" | "--quiet" => Verbosity::Quiet,
                        "-V" | "--verbose" => Verbosity::Verbose,
                        _ => unreachable!(),
                    };

                    verbosity = Some((verbosity_mode, cli_option));
                }
                "help" | "-h" | "--help" => {
                    let cli_command = match arg.as_str() {
                        "help" => CliCommand::Help,
                        "-h" => CliCommand::HelpShort,
                        "--help" => CliCommand::HelpLong,
                        _ => unreachable!(),
                    };

                    match command {
                        Some((Command::Help, previous_cli_command)) => {
                            return Err(Error {
                                args,
                                erroneous_arg_index: flag_idx,
                                kind: ErrorKind::RepeatedCommand(previous_cli_command),
                                cause: ErrorCause::CommandAlreadySelected {
                                    current: cli_command,
                                    previous: previous_cli_command,
                                },
                            });
                        }
                        Some((Command::Version, previous_cli_command)) => {
                            return Err(Error {
                                args,
                                erroneous_arg_index: flag_idx,
                                kind: ErrorKind::InvalidCommand(cli_command),
                                cause: ErrorCause::CommandCannotBeUsedAtTheSameTime {
                                    current: cli_command,
                                    previous: previous_cli_command,
                                },
                            });
                        }
                        _ => command = Some((Command::Help, cli_command)),
                    }
                }
                "version" | "-v" | "--version" => {
                    let cli_command = match arg.as_str() {
                        "version" => CliCommand::Version,
                        "-v" => CliCommand::VersionShort,
                        "--version" => CliCommand::VersionLong,
                        _ => unreachable!(),
                    };

                    match command {
                        Some((Command::Version, previous_cli_command)) => {
                            return Err(Error {
                                args,
                                erroneous_arg_index: flag_idx,
                                kind: ErrorKind::RepeatedCommand(previous_cli_command),
                                cause: ErrorCause::CommandAlreadySelected {
                                    current: cli_command,
                                    previous: previous_cli_command,
                                },
                            });
                        }
                        Some((Command::Help, previous_cli_command)) => {
                            return Err(Error {
                                args,
                                erroneous_arg_index: flag_idx,
                                kind: ErrorKind::InvalidCommand(cli_command),
                                cause: ErrorCause::CommandCannotBeUsedAtTheSameTime {
                                    current: cli_command,
                                    previous: previous_cli_command,
                                },
                            });
                        }
                        _ => command = Some((Command::Version, cli_command)),
                    }
                }
                command_str @ ("check" | "compile" | "run") => {
                    let cli_command = match command_str {
                        "check" => CliCommand::Check,
                        "compile" => CliCommand::Compile,
                        "run" => CliCommand::Run,
                        _ => unreachable!(),
                    };

                    if let Some((
                        Command::Check { .. } | Command::Compile { .. } | Command::Run { .. },
                        previous_cli_command,
                    )) = command
                    {
                        return Err(Error {
                            args,
                            erroneous_arg_index: flag_idx,
                            kind: ErrorKind::InvalidCommand(cli_command),
                            cause: ErrorCause::CommandAlreadySelected {
                                current: cli_command,
                                previous: previous_cli_command,
                            },
                        });
                    }

                    let Some((src_path_idx, src_path_str)) = other_args.next() else {
                        return Err(Error {
                            args,
                            erroneous_arg_index: flag_idx,
                            kind: ErrorKind::InvalidCommand(cli_command),
                            cause: ErrorCause::MustBeFollowedByASourceFilePath,
                        });
                    };

                    let Some(src_path) = FilePath::from(src_path_str) else {
                        let src_path: PathBuf = src_path_str.into();
                        return Err(Error {
                            args,
                            erroneous_arg_index: src_path_idx,
                            kind: ErrorKind::InvalidCommand(cli_command),
                            cause: ErrorCause::ExpectedFile { path: DirPath { inner: src_path } },
                        });
                    };

                    let mode = match command_str {
                        "check" => Command::Check { src_path },
                        "compile" | "run" => {
                            let mut out_path = None;

                            if let Some((_, out_flag)) = other_args.peek() {
                                if *out_flag == "-o" || *out_flag == "--output" {
                                    let Some((out_flag_idx, _)) = other_args.next() else {
                                        unreachable!();
                                    };

                                    let Some((out_path_idx, out_path_str)) = other_args.next()
                                    else {
                                        return Err(Error {
                                            args,
                                            erroneous_arg_index: out_flag_idx,
                                            kind: ErrorKind::InvalidOption(
                                                CliOption::OutFolderPath,
                                            ),
                                            cause: ErrorCause::MustBeFollowedByDirectoryFilePath,
                                        });
                                    };

                                    let Some(dir_path) = DirPath::from(out_path_str) else {
                                        let out_path: PathBuf = out_path_str.into();
                                        return Err(Error {
                                            args,
                                            erroneous_arg_index: out_path_idx,
                                            kind: ErrorKind::InvalidOption(
                                                CliOption::OutFolderPath,
                                            ),
                                            cause: ErrorCause::ExpectedDirectory {
                                                path: FilePath { inner: out_path },
                                            },
                                        });
                                    };

                                    out_path = Some(dir_path);
                                }
                            }

                            match command_str {
                                "compile" => Command::Compile { src_path, out_path },
                                "run" => Command::Run { src_path, out_path },
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    };

                    if let Some((Command::Help | Command::Version, _)) = command {
                        // this is just to make sure that run modes commands are properly formatted,
                        // so we do nothing in the case where the
                        // help, -h, --help, version,  -v or --version command was already selected
                    } else {
                        command = Some((mode, cli_command));
                    }
                }
                "-o" | "--output" => match other_args.next() {
                    None => {
                        return Err(Error {
                            args,
                            erroneous_arg_index: flag_idx,
                            kind: ErrorKind::InvalidOption(CliOption::OutFolderPath),
                            cause: ErrorCause::MustBeFollowedByDirectoryFilePath,
                        })
                    }
                    Some(_) => {
                        return Err(Error {
                            args,
                            erroneous_arg_index: flag_idx,
                            kind: ErrorKind::StrayOption(CliOption::OutFolderPath),
                            cause: ErrorCause::StrayOutputFolderPath,
                        })
                    }
                },
                "-c" | "--color" => {
                    let _color_mode = other_args.next();
                }
                unrecognized => {
                    let unrecognized = unrecognized.to_owned();
                    return Err(Error {
                        args,
                        erroneous_arg_index: flag_idx,
                        kind: ErrorKind::UnrecognizedArg { arg: unrecognized },
                        cause: ErrorCause::Unrecognized,
                    });
                }
            }
        }

        let verbosity = match verbosity {
            Some((verbosity, _)) => verbosity,
            None => Verbosity::Normal,
        };

        let command = match command {
            Some((command, _)) => command,
            None => Command::Help,
        };

        Ok(Self { color, verbosity, command })
    }
}

impl TryFrom<std::env::Args> for Args {
    type Error = Error;

    fn try_from(args: std::env::Args) -> Result<Self, Self::Error> {
        Self::try_from(args.collect::<Vec<String>>())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CliOption {
    ColorShort,
    ColorLong,
    QuietShort,
    QuietLong,
    VerboseShort,
    VerboseLong,
    OutFolderPath,
}

impl Display for CliOption {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ColorShort => write!(f, "-c"),
            Self::ColorLong => write!(f, "-color"),
            Self::QuietShort => write!(f, "-q"),
            Self::QuietLong => write!(f, "--quiet"),
            Self::VerboseShort => write!(f, "-V"),
            Self::VerboseLong => write!(f, "--verbose"),
            Self::OutFolderPath => write!(f, "-o"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CliCommand {
    Help,
    HelpShort,
    HelpLong,
    Version,
    VersionShort,
    VersionLong,
    Check,
    Compile,
    Run,
}

impl Display for CliCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Help => write!(f, "help"),
            Self::HelpShort => write!(f, "-h"),
            Self::HelpLong => write!(f, "--help"),
            Self::Version => write!(f, "version"),
            Self::VersionShort => write!(f, "-v"),
            Self::VersionLong => write!(f, "--version"),
            Self::Check => write!(f, "check"),
            Self::Compile => write!(f, "compile"),
            Self::Run => write!(f, "run"),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    MissingColorMode,
    UnrecognizedColorMode { mode: String },
    RepeatedOption(CliOption),
    InvalidOption(CliOption),
    RepeatedCommand(CliCommand),
    InvalidCommand(CliCommand),
    StrayOption(CliOption),
    UnrecognizedArg { arg: String },
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingColorMode => write!(f, "missing color mode"),
            Self::UnrecognizedColorMode { mode } => write!(f, "unrecognized color mode '{mode}'"),
            Self::RepeatedOption(option) => write!(f, "repeated '{option}' option"),
            Self::InvalidOption(option) => write!(f, "invalid '{option}' option"),
            Self::RepeatedCommand(command) => write!(f, "repeated '{command}' command"),
            Self::InvalidCommand(command) => write!(f, "invalid '{command}' command"),
            Self::StrayOption(option) => write!(f, "stray '{option}' option"),
            Self::UnrecognizedArg { arg } => write!(f, "unrecognized arg '{arg}'"),
        }
    }
}

#[derive(Debug)]
pub enum ErrorCause {
    CommandAlreadySelected { current: CliCommand, previous: CliCommand },
    OptionAlreadySelected { current: CliOption, previous: CliOption },
    CommandCannotBeUsedAtTheSameTime { current: CliCommand, previous: CliCommand },
    UnrecognizedColorMode,
    MustBeFollowedByColorMode,
    MustBeFollowedByASourceFilePath,
    MustBeFollowedByDirectoryFilePath,
    ExpectedFile { path: DirPath },
    ExpectedDirectory { path: FilePath },
    StrayOutputFolderPath,
    Unrecognized,
}

impl Display for ErrorCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CommandAlreadySelected { current, previous } => {
                write!(f, "cannot use '{current}' again because '{previous}' was already selected")
            }
            Self::OptionAlreadySelected { current, previous } => {
                write!(f, "cannot use '{current}' again because '{previous}' was already selected")
            }
            Self::CommandCannotBeUsedAtTheSameTime { current, previous } => {
                write!(f, "cannot use '{current}' because '{previous}' was already selected")
            }
            Self::MustBeFollowedByColorMode => {
                write!(f, "must be followed by 'auto', 'always' or 'never'")
            }
            Self::UnrecognizedColorMode => write!(f, "must be one of 'auto', 'always' or 'never'"),
            Self::MustBeFollowedByASourceFilePath => {
                write!(f, "must be followed by a source file path")
            }
            Self::MustBeFollowedByDirectoryFilePath => {
                write!(f, "must be followed by a directory file path")
            }
            Self::ExpectedFile { path } => {
                write!(f, "expected a file but got directory '{}'", path.display())
            }
            Self::ExpectedDirectory { path } => {
                write!(f, "expected a directory but got file '{}'", path.display())
            }
            Self::StrayOutputFolderPath => {
                write!(f, "output path can only be specified after a 'compile' or 'run' command")
            }
            Self::Unrecognized => write!(f, "unrecognized"),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub args: Vec<String>,
    pub erroneous_arg_index: usize,
    pub kind: ErrorKind,
    pub cause: ErrorCause,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut pointer_offset = 0;
        for arg in self.args.iter().take(self.erroneous_arg_index) {
            pointer_offset += arg.len() + 1; // + 1 to account for the space between args
        }

        let mut args = String::new();
        for arg in &self.args {
            args += &format!(" {arg}");
        }

        let msg = Colored {
            text: &self.kind.to_string(),
            fg: Fg::White,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        let pointers_len = self.args[self.erroneous_arg_index].len();
        let pointers_and_cause = Colored {
            text: format!("{spaces:^>pointers_len$} {cause}", spaces = "", cause = self.cause),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        write!(
            f,
            "{ERROR}: {msg}\
            \n{BAR}\
            \n{BAR}{args}\
            \n{BAR} {spaces:>pointer_offset$}{pointers_and_cause}",
            spaces = ""
        )
    }
}
