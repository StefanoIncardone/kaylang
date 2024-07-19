use crate::{Bg, Colored, Fg, Flag, BAR, ERROR};
use std::{
    fmt::{Display, Write as _},
    path::PathBuf,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorFlag {
    Short,
    Long,
}

impl Display for ColorFlag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Short => write!(f, "-c"),
            Self::Long => write!(f, "--color"),
        };
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Color {
    #[default]
    Auto,
    Always,
    Never,
}

impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Auto => write!(f, "auto"),
            Self::Always => write!(f, "always"),
            Self::Never => write!(f, "never"),
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ColorMode {
    pub flag: ColorFlag,
    pub color: Color,
}

impl Display for ColorMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{flag} {mode}", flag = self.flag, mode = self.color);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerbosityFlag {
    QuietShort,
    QuietLong,
    VerboseShort,
    VerboseLong,
}

impl Display for VerbosityFlag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::QuietShort => write!(f, "-q"),
            Self::QuietLong => write!(f, "--quiet"),
            Self::VerboseShort => write!(f, "-V"),
            Self::VerboseLong => write!(f, "--verbose"),
        };
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Verbosity {
    #[default]
    Normal,
    Quiet,
    Verbose,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommandFlag {
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

impl Display for CommandFlag {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFlag {
    Short,
    Long,
}

impl Display for OutputFlag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Short => write!(f, "-o"),
            Self::Long => write!(f, "--output"),
        };
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    Help { executable_name: PathBuf },
    Version,
    Check { src_path: PathBuf },
    Compile { src_path: PathBuf, out_path: Option<PathBuf> },
    Run { src_path: PathBuf, out_path: Option<PathBuf> },
}

impl Default for Command {
    fn default() -> Self {
        return Self::Help { executable_name: PathBuf::from("kay") };
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Args {
    pub color: Color,
    pub verbosity: Verbosity,
    pub command: Command,
}

impl TryFrom<Vec<String>> for Args {
    type Error = Error;

    fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
        Color::Auto.set(&std::io::stderr());

        let mut args_iter = args.iter().enumerate();
        let executable_name = match args_iter.next() {
            Some((_executable_name_index, executable_name)) => PathBuf::from(executable_name),
            None => return Err(Error::EmptyArgs),
        };

        let mut color_args = args_iter.clone();
        let mut color_mode: Option<ColorMode> = None;
        while let Some((color_flag_index, color_flag)) = color_args.next() {
            let flag = match color_flag.as_str() {
                "-c" => ColorFlag::Short,
                "--color" => ColorFlag::Long,
                _ => continue,
            };

            let Some((selected_color_index, selected_color)) = color_args.next() else {
                return Err(Error::FromArgs {
                    kind: ErrorKind::MissingColorMode(flag),
                    args,
                    erroneous_arg_index: color_flag_index,
                });
            };

            let color = match selected_color.as_str() {
                "auto" => Color::Auto,
                "always" => Color::Always,
                "never" => Color::Never,
                _ => {
                    return Err(Error::FromArgs {
                        kind: ErrorKind::UnrecognizedColorMode,
                        args,
                        erroneous_arg_index: selected_color_index,
                    })
                }
            };

            let selected_color_mode = ColorMode { flag, color };

            if let Some(previous_color_mode) = color_mode {
                return Err(Error::FromArgs {
                    kind: ErrorKind::ColorModeAlreadySelected {
                        current: selected_color_mode,
                        previous: previous_color_mode,
                    },
                    args,
                    erroneous_arg_index: selected_color_index,
                });
            }

            color_mode = Some(selected_color_mode);
        }

        let color = match color_mode {
            Some(ColorMode { color, .. }) => color,
            None => Color::Auto,
        };
        color.set(&std::io::stderr());

        let mut verbosity_mode: Option<VerbosityFlag> = None;
        let mut command_option: Option<(CommandFlag, Command)> = None;

        let mut other_args = args_iter.clone().peekable();
        while let Some((selected_flag_index, selected_flag)) = other_args.next() {
            match selected_flag.as_str() {
                "-c" | "--color" => {
                    let Some(_) = other_args.next() else {
                        unreachable!("color mode should have already been correctly parsed");
                    };
                }
                verbosity_flag @ ("-q" | "--quiet" | "-V" | "--verbose") => {
                    let flag = match verbosity_flag {
                        "-q" => VerbosityFlag::QuietShort,
                        "--quiet" => VerbosityFlag::QuietLong,
                        "-V" => VerbosityFlag::VerboseShort,
                        "--verbose" => VerbosityFlag::VerboseLong,
                        _ => unreachable!(),
                    };

                    if let Some(previous_flag) = verbosity_mode {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::VerbosityAlreadySelected {
                                current: flag,
                                previous: previous_flag,
                            },
                            args,
                            erroneous_arg_index: selected_flag_index,
                        });
                    }

                    verbosity_mode = Some(flag);
                }
                help_command @ ("help" | "-h" | "--help") => {
                    let help_flag = match help_command {
                        "help" => CommandFlag::Help,
                        "-h" => CommandFlag::HelpShort,
                        "--help" => CommandFlag::HelpLong,
                        _ => unreachable!(),
                    };

                    if let Some((previous_help_flag, Command::Help { .. } | Command::Version)) =
                        command_option
                    {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::CommandAlreadySelected {
                                current: help_flag,
                                previous: previous_help_flag,
                            },
                            args,
                            erroneous_arg_index: selected_flag_index,
                        });
                    }

                    command_option = Some((
                        help_flag,
                        Command::Help { executable_name: executable_name.clone() },
                    ));
                }
                version_command @ ("version" | "-v" | "--version") => {
                    let version_flag = match version_command {
                        "version" => CommandFlag::Version,
                        "-v" => CommandFlag::VersionShort,
                        "--version" => CommandFlag::VersionLong,
                        _ => unreachable!(),
                    };

                    if let Some((previous_version_flag, Command::Help { .. } | Command::Version)) =
                        command_option
                    {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::CommandAlreadySelected {
                                current: version_flag,
                                previous: previous_version_flag,
                            },
                            args,
                            erroneous_arg_index: selected_flag_index,
                        });
                    }

                    command_option = Some((version_flag, Command::Version));
                }
                command @ ("check" | "compile" | "run") => {
                    let command_flag = match command {
                        "check" => CommandFlag::Check,
                        "compile" => CommandFlag::Compile,
                        "run" => CommandFlag::Run,
                        _ => unreachable!(),
                    };

                    if let Some((
                        previous_command_flag,
                        Command::Check { .. } | Command::Compile { .. } | Command::Run { .. },
                    )) = command_option
                    {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::CommandAlreadySelected {
                                current: command_flag,
                                previous: previous_command_flag,
                            },
                            args,
                            erroneous_arg_index: selected_flag_index,
                        });
                    }

                    let Some((src_path_index, src_path_string)) = other_args.next() else {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::MustBeFollowedByASourceFilePath(command_flag),
                            args,
                            erroneous_arg_index: selected_flag_index,
                        });
                    };

                    let src_path = PathBuf::from(src_path_string);
                    if src_path.is_dir() {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::MustBeAFilePath(src_path),
                            args,
                            erroneous_arg_index: src_path_index,
                        });
                    }

                    let mode = match command_flag {
                        CommandFlag::Check => Command::Check { src_path },
                        CommandFlag::Compile | CommandFlag::Run => {
                            let mut out_path = None;

                            if let Some((out_flag_index, out_flag)) =
                                other_args.next_if(|(_out_flag_index, out_flag)| {
                                    return *out_flag == "-o" || *out_flag == "--output";
                                })
                            {
                                let out_option = match out_flag.as_str() {
                                    "-o" => OutputFlag::Short,
                                    "--output" => OutputFlag::Long,
                                    _ => unreachable!(),
                                };

                                let Some((out_path_index, out_path_string)) = other_args.next()
                                else {
                                    return Err(Error::FromArgs {
                                        kind: ErrorKind::MustBeFollowedByDirectoryPath(out_option),
                                        args,
                                        erroneous_arg_index: out_flag_index,
                                    });
                                };

                                let out_path_buf = PathBuf::from(out_path_string);
                                if out_path_buf.is_file() {
                                    return Err(Error::FromArgs {
                                        kind: ErrorKind::MustBeADirectoryPath(out_path_buf),
                                        args,
                                        erroneous_arg_index: out_path_index,
                                    });
                                }

                                out_path = Some(out_path_buf);
                            }

                            match command_flag {
                                CommandFlag::Compile => Command::Compile { src_path, out_path },
                                CommandFlag::Run => Command::Run { src_path, out_path },
                                CommandFlag::Help
                                | CommandFlag::HelpShort
                                | CommandFlag::HelpLong
                                | CommandFlag::Version
                                | CommandFlag::VersionShort
                                | CommandFlag::VersionLong
                                | CommandFlag::Check => unreachable!(),
                            }
                        }
                        CommandFlag::Help
                        | CommandFlag::HelpShort
                        | CommandFlag::HelpLong
                        | CommandFlag::Version
                        | CommandFlag::VersionShort
                        | CommandFlag::VersionLong => unreachable!(),
                    };

                    if let Some((_, Command::Help { .. } | Command::Version)) = command_option {
                        // this is just to make sure that run modes commands are properly formatted,
                        // so we do nothing in the case where the
                        // help, -h, --help, version,  -v or --version command was already selected
                    } else {
                        command_option = Some((command_flag, mode));
                    }
                }
                out_flag @ ("-o" | "--output") => {
                    let out_option = match out_flag {
                        "-o" => OutputFlag::Short,
                        "--output" => OutputFlag::Long,
                        _ => unreachable!(),
                    };

                    let Some((out_path_index, out_path_string)) = other_args.next() else {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::MustBeFollowedByDirectoryPath(out_option),
                            args,
                            erroneous_arg_index: selected_flag_index,
                        });
                    };

                    let out_path_buf = PathBuf::from(out_path_string);
                    if out_path_buf.is_file() {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::MustBeADirectoryPath(out_path_buf),
                            args,
                            erroneous_arg_index: out_path_index,
                        });
                    }

                    return Err(Error::FromArgs {
                        kind: ErrorKind::StrayOutputDirectoryOption(out_option),
                        args,
                        erroneous_arg_index: selected_flag_index,
                    });
                }
                _ => {
                    return Err(Error::FromArgs {
                        kind: ErrorKind::Unrecognized,
                        args,
                        erroneous_arg_index: selected_flag_index,
                    });
                }
            }
        }

        let verbosity = match verbosity_mode {
            Some(VerbosityFlag::QuietShort | VerbosityFlag::QuietLong) => Verbosity::Quiet,
            Some(VerbosityFlag::VerboseShort | VerbosityFlag::VerboseLong) => Verbosity::Verbose,
            None => Verbosity::Normal,
        };

        let command = match command_option {
            Some((_, command)) => command,
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

#[derive(Debug)]
pub enum ErrorKind {
    MissingColorMode(ColorFlag),
    UnrecognizedColorMode,
    ColorModeAlreadySelected { current: ColorMode, previous: ColorMode },

    VerbosityAlreadySelected { current: VerbosityFlag, previous: VerbosityFlag },

    CommandAlreadySelected { current: CommandFlag, previous: CommandFlag },

    MustBeFollowedByASourceFilePath(CommandFlag),
    MustBeAFilePath(PathBuf),
    MustBeFollowedByDirectoryPath(OutputFlag),
    MustBeADirectoryPath(PathBuf),
    StrayOutputDirectoryOption(OutputFlag),

    Unrecognized,
}

#[derive(Debug)]
pub enum Error {
    EmptyArgs,
    FromArgs { kind: ErrorKind, args: Vec<String>, erroneous_arg_index: usize },
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut args_text = String::new();
        let mut pointers_offset = 0;
        let mut pointers_count = 1;
        let mut error_message = String::new();
        let mut error_cause_message = String::new();

        match self {
            Self::EmptyArgs => {
                _ = write!(error_message, "missing executable name");
                _ = write!(error_cause_message, "executable name should always be present");
            }
            Self::FromArgs { args, erroneous_arg_index, kind } => {
                let executable_name = &args[0];
                _ = write!(args_text, "{executable_name}");

                // skipping the executable name
                for arg in &args[1..] {
                    _ = write!(args_text, " {arg}");
                }

                for arg in args.iter().take(*erroneous_arg_index) {
                    pointers_offset += arg.len() + 1; // + 1 to account for the space between args
                }

                let erroneous_arg = &args[*erroneous_arg_index];
                pointers_count = erroneous_arg.len();

                match kind {
                    ErrorKind::MissingColorMode(flag) => {
                        _ = write!(error_message, "missing color mode");
                        _ = write!(
                            error_cause_message,
                            "{flag} must be followed by '{auto}', '{always}' or '{never}'",
                            auto = Color::Auto,
                            always = Color::Always,
                            never = Color::Never,
                        );
                    }
                    ErrorKind::UnrecognizedColorMode => {
                        _ = write!(error_message, "unrecognized color mode '{erroneous_arg}'");
                        _ = write!(
                            error_cause_message,
                            "must be one of '{auto}', '{always}' or '{never}'",
                            auto = Color::Auto,
                            always = Color::Always,
                            never = Color::Never,
                        );
                    }
                    ErrorKind::ColorModeAlreadySelected { current, previous } => {
                        _ = write!(error_message, "repeated '{current}' color mode");
                        _ = write!(
                            error_cause_message,
                            "cannot use '{current}' because '{previous}' was already selected"
                        );
                    }
                    ErrorKind::VerbosityAlreadySelected { current, previous } => {
                        _ = write!(error_message, "repeated '{current}' verbosity mode");
                        _ = write!(
                            error_cause_message,
                            "cannot use '{current}' because '{previous}' was already selected"
                        );
                    }
                    ErrorKind::CommandAlreadySelected { current, previous } => {
                        _ = write!(error_message, "repeated '{current}' command");
                        _ = write!(
                            error_cause_message,
                            "cannot use '{current}' because '{previous}' was already selected"
                        );
                    }
                    ErrorKind::MustBeFollowedByASourceFilePath(command) => {
                        _ = write!(error_message, "invalid '{command}' command");
                        _ = write!(
                            error_cause_message,
                            "'{command}' must be followed by a file path"
                        );
                    }
                    ErrorKind::MustBeAFilePath(path) => {
                        _ = write!(error_message, "invalid '{}' path", path.display());
                        _ = write!(error_cause_message, "'{}' must be a file path", path.display());
                    }
                    ErrorKind::MustBeFollowedByDirectoryPath(option) => {
                        _ = write!(error_message, "invalid '{option}' option");
                        _ = write!(
                            error_cause_message,
                            "'{option}' must be followed by a directory path"
                        );
                    }
                    ErrorKind::MustBeADirectoryPath(path) => {
                        _ = write!(error_message, "invalid '{}' path", path.display());
                        _ = write!(
                            error_cause_message,
                            "'{}' must be a directory path",
                            path.display()
                        );
                    }
                    ErrorKind::StrayOutputDirectoryOption(option) => {
                        _ = write!(error_message, "stray '{option}' option");
                        _ = write!(
                            error_cause_message,
                            "'{option}' can only be used after a 'compile' or 'run' command"
                        );
                    }
                    ErrorKind::Unrecognized => {
                        _ = write!(error_message, "unrecognized '{erroneous_arg}' arg");
                        _ = write!(error_cause_message, "unrecognized");
                    }
                };
            }
        };

        let msg =
            Colored { text: error_message, fg: Fg::White, bg: Bg::Default, flags: Flag::Bold };

        let pointers_and_cause = Colored {
            text: format!("{spaces:^>pointers_count$} {error_cause_message}", spaces = ""),
            fg: Fg::LightRed,
            bg: Bg::Default,
            flags: Flag::Bold,
        };

        return write!(
            f,
            "{ERROR}: {msg}\
            \n{BAR}\
            \n{BAR} {args_text}\
            \n{BAR} {spaces:>pointers_offset$}{pointers_and_cause}",
            spaces = ""
        );
    }
}
