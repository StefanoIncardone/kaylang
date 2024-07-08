use crate::{Bg, Color, Colored, Command, Fg, Flag, Verbosity, BAR, ERROR};
use std::{
    fmt::{Display, Write as _},
    path::PathBuf,
};

#[derive(Debug, Default, Clone)]
pub struct Args {
    pub color: Color,
    pub verbosity: Verbosity,
    pub command: Command,
}

// TODO(stefano): create implementations than accept the name of the executable
impl TryFrom<Vec<String>> for Args {
    type Error = Error;

    // TODO(stefano): split patterns and avoid unreachable macro calls by factoring to functions
    fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
        let mut args_iter = args.iter();
        // TODO(stefano): implement an error that fires when no executable name is present
        let executable_name = match args_iter.next() {
            Some(name) => PathBuf::from(name),
            None => unreachable!("executable name is always present"),
        };

        let mut color_args = args_iter.clone().enumerate();

        Color::Auto.set(&std::io::stderr());
        let mut color_option: Option<(Color, CliOption)> = None;

        while let Some((color_flag_idx, arg)) = color_args.next() {
            let cli_option = match arg.as_str() {
                "-c" => CliOption::ColorShort,
                "--color" => CliOption::ColorLong,
                _ => continue,
            };

            if let Some((_mode, previous_cli_option)) = color_option {
                return Err(Error {
                    kind: ErrorKind::RepeatedOption(cli_option),
                    cause: ErrorCause::OptionAlreadySelected {
                        current: cli_option,
                        previous: previous_cli_option,
                    },
                    args,
                    erroneous_arg_index: color_flag_idx,
                });
            }

            let Some((color_mode_idx, mode)) = color_args.next() else {
                return Err(Error {
                    kind: ErrorKind::MissingColorMode,
                    cause: ErrorCause::MustBeFollowedByColorMode,
                    args,
                    erroneous_arg_index: color_flag_idx,
                });
            };

            let color_mode = match mode.as_str() {
                "auto" => Color::Auto,
                "always" => Color::Always,
                "never" => Color::Never,
                unrecognized => {
                    return Err(Error {
                        kind: ErrorKind::UnrecognizedColorMode { mode: unrecognized.to_owned() },
                        cause: ErrorCause::UnrecognizedColorMode,
                        args,
                        erroneous_arg_index: color_mode_idx,
                    })
                }
            };

            color_option = Some((color_mode, cli_option));
        }

        let color = match color_option {
            Some((mode, _)) => mode,
            None => Color::Auto,
        };
        color.set(&std::io::stderr());

        let mut verbosity_option: Option<(Verbosity, CliOption)> = None;
        let mut command_option: Option<(Command, CliCommand)> = None;

        let mut other_args = args_iter.clone().enumerate().peekable();

        while let Some((flag_idx, arg)) = other_args.next() {
            match arg.as_str() {
                quiet_flag @ ("-q" | "--quiet" | "-V" | "--verbose") => {
                    let cli_option = match quiet_flag {
                        "-q" => CliOption::QuietShort,
                        "--quiet" => CliOption::QuietLong,
                        "-V" => CliOption::VerboseShort,
                        "--verbose" => CliOption::VerboseLong,
                        _ => unreachable!(),
                    };

                    if let Some((_mode, previous_cli_option)) = verbosity_option {
                        return Err(Error {
                            kind: ErrorKind::RepeatedOption(cli_option),
                            cause: ErrorCause::OptionAlreadySelected {
                                current: cli_option,
                                previous: previous_cli_option,
                            },
                            args,
                            erroneous_arg_index: flag_idx,
                        });
                    }

                    let verbosity_mode = match arg.as_str() {
                        "-q" | "--quiet" => Verbosity::Quiet,
                        "-V" | "--verbose" => Verbosity::Verbose,
                        _ => unreachable!(),
                    };

                    verbosity_option = Some((verbosity_mode, cli_option));
                }
                help_command @ ("help" | "-h" | "--help") => {
                    let cli_command = match help_command {
                        "help" => CliCommand::Help,
                        "-h" => CliCommand::HelpShort,
                        "--help" => CliCommand::HelpLong,
                        _ => unreachable!(),
                    };

                    match command_option {
                        Some((Command::Help { .. }, previous_cli_command)) => {
                            return Err(Error {
                                kind: ErrorKind::RepeatedCommand(previous_cli_command),
                                cause: ErrorCause::CommandAlreadySelected {
                                    current: cli_command,
                                    previous: previous_cli_command,
                                },
                                args,
                                erroneous_arg_index: flag_idx,
                            });
                        }
                        Some((Command::Version, previous_cli_command)) => {
                            return Err(Error {
                                kind: ErrorKind::InvalidCommand(cli_command),
                                cause: ErrorCause::CommandCannotBeUsedAtTheSameTime {
                                    current: cli_command,
                                    previous: previous_cli_command,
                                },
                                args,
                                erroneous_arg_index: flag_idx,
                            });
                        }
                        _ => {
                            command_option = Some((
                                Command::Help { executable_name: executable_name.clone() },
                                cli_command,
                            ));
                        }
                    }
                }
                version_command @ ("version" | "-v" | "--version") => {
                    let cli_command = match version_command {
                        "version" => CliCommand::Version,
                        "-v" => CliCommand::VersionShort,
                        "--version" => CliCommand::VersionLong,
                        _ => unreachable!(),
                    };

                    match command_option {
                        Some((Command::Version, previous_cli_command)) => {
                            return Err(Error {
                                kind: ErrorKind::RepeatedCommand(previous_cli_command),
                                cause: ErrorCause::CommandAlreadySelected {
                                    current: cli_command,
                                    previous: previous_cli_command,
                                },
                                args,
                                erroneous_arg_index: flag_idx,
                            });
                        }
                        Some((Command::Help { .. }, previous_cli_command)) => {
                            return Err(Error {
                                kind: ErrorKind::InvalidCommand(cli_command),
                                cause: ErrorCause::CommandCannotBeUsedAtTheSameTime {
                                    current: cli_command,
                                    previous: previous_cli_command,
                                },
                                args,
                                erroneous_arg_index: flag_idx,
                            });
                        }
                        _ => command_option = Some((Command::Version, cli_command)),
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
                    )) = command_option
                    {
                        return Err(Error {
                            kind: ErrorKind::InvalidCommand(cli_command),
                            cause: ErrorCause::CommandAlreadySelected {
                                current: cli_command,
                                previous: previous_cli_command,
                            },
                            args,
                            erroneous_arg_index: flag_idx,
                        });
                    }

                    let Some((_src_path_idx, src_path_str)) = other_args.next() else {
                        return Err(Error {
                            kind: ErrorKind::InvalidCommand(cli_command),
                            cause: ErrorCause::MustBeFollowedByASourceFilePath,
                            args,
                            erroneous_arg_index: flag_idx,
                        });
                    };

                    let src_path = PathBuf::from(src_path_str);

                    let mode = match command_str {
                        "check" => Command::Check { src_path },
                        "compile" | "run" => {
                            let mut out_path = None;

                            if let Some((_, out_flag)) = other_args.peek() {
                                if *out_flag == "-o" || *out_flag == "--output" {
                                    let Some((out_flag_idx, _)) = other_args.next() else {
                                        unreachable!();
                                    };

                                    let Some((_out_path_idx, out_path_str)) = other_args.next()
                                    else {
                                        return Err(Error {
                                            kind: ErrorKind::InvalidOption(
                                                CliOption::OutFolderPath,
                                            ),
                                            cause: ErrorCause::MustBeFollowedByDirectoryFilePath,
                                            args,
                                            erroneous_arg_index: out_flag_idx,
                                        });
                                    };

                                    out_path = Some(PathBuf::from(out_path_str));
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

                    if let Some((Command::Help { .. } | Command::Version, _)) = command_option {
                        // this is just to make sure that run modes commands are properly formatted,
                        // so we do nothing in the case where the
                        // help, -h, --help, version,  -v or --version command was already selected
                    } else {
                        command_option = Some((mode, cli_command));
                    }
                }
                "-o" | "--output" => match other_args.next() {
                    None => {
                        return Err(Error {
                            kind: ErrorKind::InvalidOption(CliOption::OutFolderPath),
                            cause: ErrorCause::MustBeFollowedByDirectoryFilePath,
                            args,
                            erroneous_arg_index: flag_idx,
                        })
                    }
                    Some(_) => {
                        return Err(Error {
                            kind: ErrorKind::StrayOption(CliOption::OutFolderPath),
                            cause: ErrorCause::StrayOutputFolderPath,
                            args,
                            erroneous_arg_index: flag_idx,
                        })
                    }
                },
                "-c" | "--color" => {
                    let _color_mode = other_args.next();
                }
                unrecognized => {
                    let unrecognized_arg = unrecognized.to_owned();
                    return Err(Error {
                        kind: ErrorKind::UnrecognizedArg { arg: unrecognized_arg },
                        cause: ErrorCause::Unrecognized,
                        args,
                        erroneous_arg_index: flag_idx,
                    });
                }
            }
        }

        let verbosity = match verbosity_option {
            Some((verbosity, _)) => verbosity,
            None => Verbosity::Normal,
        };

        let command = match command_option {
            Some((command, _)) => command,
            None => Command::Help { executable_name },
        };

        return Ok(Self { color, verbosity, command });
    }
}

impl TryFrom<std::env::Args> for Args {
    type Error = Error;

    fn try_from(args: std::env::Args) -> Result<Self, Self::Error> {
        return Self::try_from(args.collect::<Vec<String>>());
    }
}

#[derive(Debug, Clone, Copy)]
#[allow(clippy::module_name_repetitions)] // would collide with the built-in Option enum
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
        return match self {
            Self::ColorShort => write!(f, "-c"),
            Self::ColorLong => write!(f, "-color"),
            Self::QuietShort => write!(f, "-q"),
            Self::QuietLong => write!(f, "--quiet"),
            Self::VerboseShort => write!(f, "-V"),
            Self::VerboseLong => write!(f, "--verbose"),
            Self::OutFolderPath => write!(f, "-o"),
        };
    }
}

#[derive(Debug, Clone, Copy)]
#[allow(clippy::module_name_repetitions)] // would collide with std::process::Command
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
        return match self {
            Self::Help => write!(f, "help"),
            Self::HelpShort => write!(f, "-h"),
            Self::HelpLong => write!(f, "--help"),
            Self::Version => write!(f, "version"),
            Self::VersionShort => write!(f, "-v"),
            Self::VersionLong => write!(f, "--version"),
            Self::Check => write!(f, "check"),
            Self::Compile => write!(f, "compile"),
            Self::Run => write!(f, "run"),
        };
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
        return match self {
            Self::MissingColorMode => write!(f, "missing color mode"),
            Self::UnrecognizedColorMode { mode } => write!(f, "unrecognized color mode '{mode}'"),
            Self::RepeatedOption(option) => write!(f, "repeated '{option}' option"),
            Self::InvalidOption(option) => write!(f, "invalid '{option}' option"),
            Self::RepeatedCommand(command) => write!(f, "repeated '{command}' command"),
            Self::InvalidCommand(command) => write!(f, "invalid '{command}' command"),
            Self::StrayOption(option) => write!(f, "stray '{option}' option"),
            Self::UnrecognizedArg { arg } => write!(f, "unrecognized arg '{arg}'"),
        };
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
    StrayOutputFolderPath,
    Unrecognized,
}

impl Display for ErrorCause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
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
            Self::StrayOutputFolderPath => {
                write!(f, "output path can only be specified after a 'compile' or 'run' command")
            }
            Self::Unrecognized => write!(f, "unrecognized"),
        };
    }
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub cause: ErrorCause,
    pub args: Vec<String>,
    pub erroneous_arg_index: usize,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut pointer_offset = 0;
        for arg in self.args.iter().take(self.erroneous_arg_index) {
            pointer_offset += arg.len() + 1; // + 1 to account for the space between args
        }

        let mut args = String::new();
        let mut args_iter = self.args.iter();
        let Some(last_arg) = args_iter.next_back() else {
            unreachable!("program executable name should always be present");
        };

        for arg in args_iter {
            _ = write!(args, "{arg} ");
        }
        args += last_arg.as_str();

        let msg = Colored {
            text: &self.kind.to_string(),
            fg: Fg::White,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        let pointers_len = match self.args.get(self.erroneous_arg_index) {
            Some(erroneous_arg) => erroneous_arg.len(),
            None => 0,
        };

        let pointers_and_cause = Colored {
            text: format!("{spaces:^>pointers_len$} {cause}", spaces = "", cause = self.cause),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        return write!(
            f,
            "{ERROR}: {msg}\
            \n{BAR}\
            \n{BAR} {args}\
            \n{BAR} {spaces:>pointer_offset$}{pointers_and_cause}",
            spaces = ""
        );
    }
}
