#![warn(clippy::print_stdout, clippy::print_stderr)]

pub mod color;
pub mod compiler;
pub mod src_file;
pub mod syntax;

use color::{Bg, Colored, Fg, Flag, Flags};
use std::{
    fmt::{Display, Write as _},
    path::{Path, PathBuf},
    time::Instant,
};

// help and version messages
const HELP_FG: Fg = Fg::White;
const HELP_BG: Bg = Bg::Default;
const HELP_FLAGS: Flags = Flag::Bold;

#[rustfmt::skip] pub(crate) static VERSION:   Colored<&str> = Colored { text: env!("CARGO_PKG_VERSION"), fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static USAGE:     Colored<&str> = Colored { text: "Usage",                   fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static OPTIONS:   Colored<&str> = Colored { text: "Options",                 fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static COMMAND:   Colored<&str> = Colored { text: "Command",                 fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static MODE:      Colored<&str> = Colored { text: "mode",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static FILE:      Colored<&str> = Colored { text: "file",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static PATH:      Colored<&str> = Colored { text: "path",                    fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static OUTPUT:    Colored<&str> = Colored { text: "Output",                  fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };
#[rustfmt::skip] pub(crate) static VERBOSITY: Colored<&str> = Colored { text: "Verbosity",               fg: HELP_FG, bg: HELP_BG, flags: HELP_FLAGS };

// main compilation steps (displayed when verbosity level is normal or verbose)
const STEP_FG: Fg = Fg::LightGreen;
const STEP_BG: Bg = Bg::Default;
const STEP_FLAGS: Flags = Flag::Bold;
const STEP_INDENT: usize = 0;
const STEP_PADDING: usize = 9;

#[rustfmt::skip] pub static CHECKING:  Colored<&str> = Colored { text: "Checking",  fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static COMPILING: Colored<&str> = Colored { text: "Compiling", fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static RUNNING:   Colored<&str> = Colored { text: "Running",   fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static DONE:      Colored<&str> = Colored { text: "Done",      fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };

// sub compilation steps (displayed when verbosity lever is verbose)
const SUBSTEP_FG: Fg = Fg::LightBlue;
const SUBSTEP_BG: Bg = Bg::Default;
const SUBSTEP_FLAGS: Flags = Flag::Bold;
const SUBSTEP_INDENT: usize = 4;
const SUBSTEP_PADDING: usize = 14;

#[rustfmt::skip] pub static LOADING_SOURCE: Colored<&str> = Colored { text: "Loding Source",  fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static TOKENIZATION:   Colored<&str> = Colored { text: "Tokenizing",     fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static BUILDING_AST:   Colored<&str> = Colored { text: "Building Ast",   fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static GENERATING_ASM: Colored<&str> = Colored { text: "Generating asm", fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static ASSEMBLING:     Colored<&str> = Colored { text: "Assembling",     fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static LINKING:        Colored<&str> = Colored { text: "Linking",        fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static SUBSTEP_DONE:   Colored<&str> = Colored { text: "Done",           fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };

// errors
const ERR_FG: Fg = Fg::LightRed;
const ERR_BG: Bg = Bg::Default;
const ERR_FLAGS: Flags = Flag::Bold;

const BAR_FG: Fg = Fg::LightBlue;
const BAR_BG: Bg = Bg::Default;
const BAR_FLAGS: Flags = Flag::Bold;

#[rustfmt::skip] pub(crate) static ERROR: Colored<&str> = Colored { text: "Error",  fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static CAUSE: Colored<&str> = Colored { text: "Cause",  fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static AT:    Colored<&str> = Colored { text: "at",     fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static BAR:   Colored<&str> = Colored { text: "|",      fg: BAR_FG, bg: BAR_BG, flags: BAR_FLAGS };

#[rustfmt::skip] pub static COULD_NOT_RUN_ASSEMBLER:  Colored<&str> = Colored { text: "Could not run assembler",  fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static COULD_NOT_RUN_LINKER:     Colored<&str> = Colored { text: "Could not run linker",     fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static COULD_NOT_RUN_EXECUTABLE: Colored<&str> = Colored { text: "Could not run executable", fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static ASSEMBLING_ERROR:         Colored<&str> = Colored { text: "Assembling Error",         fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static LINKING_ERROR:            Colored<&str> = Colored { text: "Linking Error",            fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };

#[derive(Debug)]
pub struct Logger {
    pub start: Instant,
}

#[allow(clippy::new_without_default)]
impl Logger {
    #[inline(always)]
    #[must_use]
    pub fn new() -> Self {
        return Self { start: Instant::now() };
    }
}

// logging without verbosity information, intended for use in specialized cases
#[allow(clippy::print_stderr)]
impl Logger {
    pub fn info<Text: AsRef<str>>(step: &Colored<Text>, path: &Path) {
        eprintln!(
            "{spaces:STEP_INDENT$}{step:>STEP_PADDING$}: {path}",
            spaces = "",
            path = path.display()
        );
    }

    fn done<Text: AsRef<str>>(self, step: &Colored<Text>, indent: usize, padding: usize) {
        let elapsed_time = Colored {
            text: format!("{}s", self.start.elapsed().as_secs_f32()),
            fg: Fg::White,
            ..Default::default()
        };

        eprintln!("{spaces:indent$}{step:>padding$}: in {elapsed_time}", spaces = "");
    }

    pub fn step_done(self) {
        self.done(&DONE, STEP_INDENT, STEP_PADDING);
    }

    pub fn sub_step_done<Text: AsRef<str>>(self, sub_step: &Colored<Text>) {
        self.done(sub_step, SUBSTEP_INDENT, SUBSTEP_PADDING);
    }
}

// logging with verbosity information, intended for use in general cases
impl Logger {
    pub fn info_with_verbosity<Text: AsRef<str>>(
        step: &Colored<Text>,
        path: &Path,
        verbosity: Verbosity,
    ) {
        if let Verbosity::Normal | Verbosity::Verbose = verbosity {
            Self::info(step, path);
        }
    }

    pub fn step_done_with_verbosity(self, verbosity: Verbosity) {
        if let Verbosity::Normal | Verbosity::Verbose = verbosity {
            self.done(&DONE, STEP_INDENT, STEP_PADDING);
        }
    }

    pub fn sub_step_done_with_verbosity<Text: AsRef<str>>(
        self,
        sub_step: &Colored<Text>,
        verbosity: Verbosity,
    ) {
        if let Verbosity::Verbose = verbosity {
            self.done(sub_step, SUBSTEP_INDENT, SUBSTEP_PADDING);
        }
    }
}

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    Version,
    Help { executable_name: PathBuf },
    Check { src_path: PathBuf, verbosity: Verbosity },
    Compile { src_path: PathBuf, out_path: Option<PathBuf>, verbosity: Verbosity },
    Run { src_path: PathBuf, out_path: Option<PathBuf>, verbosity: Verbosity },
}

impl Default for Command {
    fn default() -> Self {
        return Self::Help { executable_name: PathBuf::from("kay") };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Version {
    pub color: Color,
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.color.set(&std::io::stdout());
        return write!(f, "Kaylang compiler, version {VERSION}");
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Help {
    pub executable_name: PathBuf,
    pub color: Color,
}

impl Display for Help {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            r"{Version}

{USAGE}: {executable_name} [{OPTIONS}] [{COMMAND}]

{OPTIONS}:
    {_c}, {__color} <{MODE}>    Wether to display colored output ({MODE}: {auto} (default), {always}, {never})

{COMMAND}s:
    {help},    {_h}, {__help}                     Display this message (default)
    {version}, {_v}, {__version}                  Display the compiler version
    {check}    <{FILE}>          [{VERBOSITY}]    Check the source code for correctness
    {compile}  <{FILE}> [{OUTPUT}] [{VERBOSITY}]    Compile the source code down to an executable
    {run}      <{FILE}> [{OUTPUT}] [{VERBOSITY}]    Compile and run the generated executable

    {VERBOSITY}:
        {_q}, {__quiet}     Don't display any compilation information
        {_V}, {__verbose}   Display extra compilation information

    {OUTPUT}:
        {_o}, {__output} <{PATH}>   Folder to populate with compilation artifacts (default: '.')",
            Version = Version { color: self.color },
            executable_name = self.executable_name.display(),

            _c = ColorFlag::Short,
            __color = ColorFlag::Long,
            auto = Color::Auto,
            always = Color::Always,
            never = Color::Never,

            help = CommandFlag::Help,
            _h = CommandFlag::HelpShort,
            __help = CommandFlag::HelpLong,

            version = CommandFlag::Version,
            _v = CommandFlag::VersionShort,
            __version = CommandFlag::VersionLong,

            check = CommandFlag::Check,
            compile = CommandFlag::Compile,
            run = CommandFlag::Run,

            _o = OutputFlag::Short,
            __output = OutputFlag::Long,

            _q = VerbosityFlag::QuietShort,
            __quiet = VerbosityFlag::QuietLong,
            _V = VerbosityFlag::VerboseShort,
            __verbose = VerbosityFlag::VerboseLong,
        );
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Args {
    pub color: Color,
    pub command: Command,
}

impl TryFrom<Vec<String>> for Args {
    type Error = Error;

    fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
        fn is_verbosity_flag((_verbosity_flag_index, verbosity_flag): &(usize, &String)) -> bool {
            return matches!(verbosity_flag.as_str(), "-q" | "--quiet" | "-V" | "--verbose");
        }

        #[allow(clippy::single_call_fn)]
        fn is_out_flag((_out_flag_index, out_flag): &(usize, &String)) -> bool {
            return matches!(out_flag.as_str(), "-o" | "--output");
        }

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

        let mut command_option: Option<(CommandFlag, Command)> = None;

        let mut other_args = args_iter.clone().peekable();
        while let Some((selected_flag_index, selected_flag)) = other_args.next() {
            match selected_flag.as_str() {
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
                "check" => {
                    let command_flag = CommandFlag::Check;

                    let Some((src_path_index, src_path_string)) = other_args.next() else {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::MustBeFollowedByASourceFilePath(command_flag),
                            args,
                            erroneous_arg_index: selected_flag_index,
                        });
                    };

                    let src_path = PathBuf::from(src_path_string);
                    if !src_path.is_file() {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::MustBeAFilePath(src_path),
                            args,
                            erroneous_arg_index: src_path_index,
                        });
                    }

                    let verbosity = if let Some((_verbosity_flag_index, verbosity_flag)) =
                        other_args.next_if(is_verbosity_flag)
                    {
                        match verbosity_flag.as_str() {
                            "-q" | "--quiet" => Verbosity::Quiet,
                            "-V" | "--verbose" => Verbosity::Verbose,
                            _ => unreachable!(),
                        }
                    } else {
                        Verbosity::Normal
                    };

                    match &command_option {
                        None => {
                            let mode = Command::Check { src_path, verbosity };
                            command_option = Some((command_flag, mode));
                        }
                        Some((previous_command_flag, previous_command)) => match previous_command {
                            Command::Help { .. } | Command::Version => {
                                // this is just to make sure that run modes commands are properly
                                // formatted, so we do nothing in the case where the
                                // help, -h, --help, version, -v or --version command was already
                                // selected
                            }
                            Command::Check { .. }
                            | Command::Compile { .. }
                            | Command::Run { .. } => {
                                return Err(Error::FromArgs {
                                    kind: ErrorKind::CommandAlreadySelected {
                                        current: command_flag,
                                        previous: *previous_command_flag,
                                    },
                                    args,
                                    erroneous_arg_index: selected_flag_index,
                                });
                            }
                        },
                    }
                }
                command @ ("compile" | "run") => {
                    let command_flag = match command {
                        "compile" => CommandFlag::Compile,
                        "run" => CommandFlag::Run,
                        _ => unreachable!(),
                    };

                    let Some((src_path_index, src_path_string)) = other_args.next() else {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::MustBeFollowedByASourceFilePath(command_flag),
                            args,
                            erroneous_arg_index: selected_flag_index,
                        });
                    };

                    let src_path = PathBuf::from(src_path_string);
                    if !src_path.is_file() {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::MustBeAFilePath(src_path),
                            args,
                            erroneous_arg_index: src_path_index,
                        });
                    }

                    let out_path =
                        if let Some((out_flag_index, out_flag)) = other_args.next_if(is_out_flag) {
                            let out_option = match out_flag.as_str() {
                                "-o" => OutputFlag::Short,
                                "--output" => OutputFlag::Long,
                                _ => unreachable!(),
                            };

                            let Some((out_path_index, out_path_string)) = other_args.next() else {
                                return Err(Error::FromArgs {
                                    kind: ErrorKind::MustBeFollowedByDirectoryPath(out_option),
                                    args,
                                    erroneous_arg_index: out_flag_index,
                                });
                            };

                            let out_path_buf = PathBuf::from(out_path_string);
                            if !out_path_buf.is_dir() {
                                return Err(Error::FromArgs {
                                    kind: ErrorKind::MustBeADirectoryPath(out_path_buf),
                                    args,
                                    erroneous_arg_index: out_path_index,
                                });
                            }

                            Some(out_path_buf)
                        } else {
                            None
                        };

                    let verbosity = if let Some((_verbosity_flag_index, verbosity_flag)) =
                        other_args.next_if(is_verbosity_flag)
                    {
                        match verbosity_flag.as_str() {
                            "-q" | "--quiet" => Verbosity::Quiet,
                            "-V" | "--verbose" => Verbosity::Verbose,
                            _ => unreachable!(),
                        }
                    } else {
                        Verbosity::Normal
                    };

                    match &command_option {
                        None => {
                            let mode = match command_flag {
                                CommandFlag::Compile => {
                                    Command::Compile { src_path, out_path, verbosity }
                                }
                                CommandFlag::Run => Command::Run { src_path, out_path, verbosity },
                                CommandFlag::Help
                                | CommandFlag::HelpShort
                                | CommandFlag::HelpLong
                                | CommandFlag::Version
                                | CommandFlag::VersionShort
                                | CommandFlag::VersionLong
                                | CommandFlag::Check => unreachable!(),
                            };

                            command_option = Some((command_flag, mode));
                        }
                        Some((previous_command_flag, previous_command)) => match previous_command {
                            Command::Help { .. } | Command::Version => {
                                // this is just to make sure that run modes commands are properly
                                // formatted, so we do nothing in the case where the
                                // help, -h, --help, version, -v or --version command was already
                                // selected
                            }
                            Command::Check { .. }
                            | Command::Compile { .. }
                            | Command::Run { .. } => {
                                return Err(Error::FromArgs {
                                    kind: ErrorKind::CommandAlreadySelected {
                                        current: command_flag,
                                        previous: *previous_command_flag,
                                    },
                                    args,
                                    erroneous_arg_index: selected_flag_index,
                                });
                            }
                        },
                    }
                }
                out_flag @ ("-o" | "--output") => {
                    let flag = match out_flag {
                        "-o" => OutputFlag::Short,
                        "--output" => OutputFlag::Long,
                        _ => unreachable!(),
                    };

                    let Some((out_path_index, out_path_string)) = other_args.next() else {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::MustBeFollowedByDirectoryPath(flag),
                            args,
                            erroneous_arg_index: selected_flag_index,
                        });
                    };

                    let out_path_buf = PathBuf::from(out_path_string);
                    if !out_path_buf.is_dir() {
                        return Err(Error::FromArgs {
                            kind: ErrorKind::MustBeADirectoryPath(out_path_buf),
                            args,
                            erroneous_arg_index: out_path_index,
                        });
                    }

                    return Err(Error::FromArgs {
                        kind: ErrorKind::StrayOutputDirectoryOption(flag),
                        args,
                        erroneous_arg_index: selected_flag_index,
                    });
                }
                verbosity_flag @ ("-q" | "--quiet" | "-V" | "--verbose") => {
                    let flag = match verbosity_flag {
                        "-q" => VerbosityFlag::QuietShort,
                        "--quiet" => VerbosityFlag::QuietLong,
                        "-V" => VerbosityFlag::VerboseShort,
                        "--verbose" => VerbosityFlag::VerboseLong,
                        _ => unreachable!(),
                    };

                    return Err(Error::FromArgs {
                        kind: ErrorKind::StrayVerbosityOption(flag),
                        args,
                        erroneous_arg_index: selected_flag_index,
                    });
                }
                "-c" | "--color" => {
                    let Some(_) = other_args.next() else {
                        unreachable!("color mode should have already been correctly parsed");
                    };
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

        let command = match command_option {
            Some((_, command)) => command,
            None => Command::Help { executable_name },
        };

        return Ok(Self { color, command });
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

    CommandAlreadySelected { current: CommandFlag, previous: CommandFlag },

    MustBeFollowedByASourceFilePath(CommandFlag),
    MustBeAFilePath(PathBuf),
    MustBeFollowedByDirectoryPath(OutputFlag),
    MustBeADirectoryPath(PathBuf),

    StrayVerbosityOption(VerbosityFlag),
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
                    ErrorKind::StrayVerbosityOption(option) => {
                        _ = write!(error_message, "stray '{option}' option");
                        _ = write!(
                            error_cause_message,
                            "'{option}' can only be used after a 'check', 'compile' or 'run' command"
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
