// IDEA(stefano): raise an error or warn on missing `-o`/`--output` that will pollute the current directory
#![warn(clippy::print_stdout, clippy::print_stderr)]

pub mod back_end;
pub mod color;
pub mod error;
pub mod front_end;

use color::{ansi_flag, AnsiFlag, Bg, Colored, Fg};
use core::fmt::{Display, Write as _};
use error::MsgWithCauseUnderText;
use front_end::src_file::column32;
use std::{
    io::IsTerminal, path::{Path, PathBuf}, time::Instant
};

const fn max_text_len(texts: &[&str]) -> usize {
    let mut max_len = 0;
    let mut text_index = 0;
    while text_index < texts.len() {
        let text = texts[text_index];
        let text_len = text.len();
        if text_len > max_len {
            max_len = text_len;
        }
        text_index += 1;
    }
    return max_len;
}

// help and version messages
const HELP_FG: Fg = Fg::White;
const HELP_BG: Bg = Bg::Default;
const HELP_FLAGS: ansi_flag = AnsiFlag::Bold as ansi_flag;

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
const STEP_FLAGS: ansi_flag = AnsiFlag::Bold as ansi_flag;
const STEP_INDENT: usize = 0;
static STEP_PADDING: usize =
    max_text_len(&[CHECKING.text, COMPILING.text, RUNNING.text, DONE.text]);

#[rustfmt::skip] pub static CHECKING:  Colored<&str> = Colored { text: "Checking",  fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static COMPILING: Colored<&str> = Colored { text: "Compiling", fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static RUNNING:   Colored<&str> = Colored { text: "Running",   fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };
#[rustfmt::skip] pub static DONE:      Colored<&str> = Colored { text: "Done",      fg: STEP_FG, bg: STEP_BG, flags: STEP_FLAGS };

// sub compilation steps (displayed when verbosity lever is verbose)
const SUBSTEP_FG: Fg = Fg::LightBlue;
const SUBSTEP_BG: Bg = Bg::Default;
const SUBSTEP_FLAGS: ansi_flag = AnsiFlag::Bold as ansi_flag;
const SUBSTEP_INDENT: usize = STEP_INDENT + 4;
static SUBSTEP_PADDING: usize = max_text_len(&[
    LOADING_SOURCE.text,
    TOKENIZATION.text,
    PARSING_SYNTAX_TREE.text,
    PARSING_AST.text,
    GENERATING_ASM.text,
    ASSEMBLING.text,
    LINKING.text,
    SUBSTEP_DONE.text,
]);

#[rustfmt::skip] pub static LOADING_SOURCE:      Colored<&str> = Colored { text: "Loading Source",      fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static TOKENIZATION:        Colored<&str> = Colored { text: "Tokenizing",          fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static PARSING_SYNTAX_TREE: Colored<&str> = Colored { text: "Parsing Syntax Tree", fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static PARSING_AST:         Colored<&str> = Colored { text: "Parsing Ast",         fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static GENERATING_ASM:      Colored<&str> = Colored { text: "Generating asm",      fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static ASSEMBLING:          Colored<&str> = Colored { text: "Assembling",          fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static LINKING:             Colored<&str> = Colored { text: "Linking",             fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };
#[rustfmt::skip] pub static SUBSTEP_DONE:        Colored<&str> = Colored { text: "Done",                fg: SUBSTEP_FG, bg: SUBSTEP_BG, flags: SUBSTEP_FLAGS };

// errors
const ERR_FG: Fg = Fg::LightRed;
const ERR_BG: Bg = Bg::Default;
const ERR_FLAGS: ansi_flag = AnsiFlag::Bold as ansi_flag;

const BAR_FG: Fg = Fg::LightBlue;
const BAR_BG: Bg = Bg::Default;
const BAR_FLAGS: ansi_flag = AnsiFlag::Bold as ansi_flag;

#[rustfmt::skip] pub(crate) static ERROR: Colored<&str> = Colored { text: "Error",  fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static CAUSE: Colored<&str> = Colored { text: "Cause",  fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static AT:    Colored<&str> = Colored { text: "at",     fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub(crate) static BAR:   Colored<&str> = Colored { text: "|",      fg: BAR_FG, bg: BAR_BG, flags: BAR_FLAGS };

// REMOVE(stefano): they should not be part of the library, as they should be provided by the users of the compiler
#[rustfmt::skip] pub static COULD_NOT_WRITE_COMPILED_CODE: Colored<&str> = Colored { text: "Could not write compile code", fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static COULD_NOT_RUN_ASSEMBLER:       Colored<&str> = Colored { text: "Could not run assembler",      fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static COULD_NOT_RUN_LINKER:          Colored<&str> = Colored { text: "Could not run linker",         fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static COULD_NOT_RUN_EXECUTABLE:      Colored<&str> = Colored { text: "Could not run executable",     fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static ASSEMBLING_ERROR:              Colored<&str> = Colored { text: "Assembling Error",             fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static LINKING_ERROR:                 Colored<&str> = Colored { text: "Linking Error",                fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };

// IDEA(stefano): move `Logger` and `Verbosity` to own module
#[derive(Debug)]
pub struct Logger {
    pub start: Instant,
}

impl Logger {
    #[must_use]
    #[inline(always)]
    pub fn new() -> Self {
        return Self { start: Instant::now() };
    }
}

impl Default for Logger {
    #[must_use]
    #[inline(always)]
    fn default() -> Self {
        return Self::new();
    }
}

#[expect(clippy::print_stderr, reason = "it's a logger")]
impl Logger {
    #[inline]
    pub fn info(text: &dyn Display, path: &Path) {
        eprintln!(
            "{spaces:STEP_INDENT$}{text:>STEP_PADDING$}: {path}",
            spaces = "",
            path = path.display()
        );
    }

    #[inline]
    pub fn info_with_verbosity(text: &dyn Display, path: &Path, verbosity: Verbosity) {
        if let Verbosity::Normal | Verbosity::Verbose = verbosity {
            Self::info(text, path);
        }
    }

    #[inline]
    pub fn done(self, text: &dyn Display, output: Option<&Path>, padding: usize) {
        let elapsed_time = Colored {
            text: format!("{:.06}s", self.start.elapsed().as_secs_f32()),
            fg: Fg::White,
            ..Default::default()
        };

        if let Some(out) = output {
            eprintln!("{text:>padding$}: in {elapsed_time} [{out}]", out = out.display());
        } else {
            eprintln!("{text:>padding$}: in {elapsed_time}");
        }
    }

    #[inline(always)]
    pub fn step(self, text: &dyn Display, output: Option<&Path>) {
        self.done(text, output, STEP_INDENT + STEP_PADDING);
    }

    #[inline(always)]
    pub fn step_with_verbosity(self, text: &dyn Display, output: Option<&Path>, verbosity: Verbosity) {
        if let Verbosity::Normal | Verbosity::Verbose = verbosity {
            self.done(text, output, STEP_INDENT + STEP_PADDING);
        }
    }

    #[inline(always)]
    pub fn sub_step(self, text: &dyn Display, output: Option<&Path>) {
        self.done(text, output, SUBSTEP_INDENT + SUBSTEP_PADDING);
    }

    #[inline(always)]
    pub fn sub_step_with_verbosity(self, text: &dyn Display, output: Option<&Path>, verbosity: Verbosity) {
        if let Verbosity::Verbose = verbosity {
            self.done(text, output, SUBSTEP_INDENT + SUBSTEP_PADDING);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorFlag {
    Short,
    Long,
}

impl Display for ColorFlag {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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

impl Color {
    pub fn set<I: IsTerminal>(self, sink: &I) {
        use crate::color::{print, print_color, print_no_color};
        unsafe {
            print = match self {
                Self::Auto if sink.is_terminal() => print_color,
                Self::Auto => print_no_color,
                Self::Always => print_color,
                Self::Never => print_no_color,
            }
        }
    }
}

impl Display for Color {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return write!(f, "{flag} {mode}", flag = self.flag, mode = self.color);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommandFlag {
    Help,
    HelpShort,
    HelpLong,
    HelpQuestion,
    HelpQuestionShort,
    HelpQuestionLong,
    Version,
    VersionShort,
    VersionLong,
    Check,
    Compile,
    Run,
}

impl Display for CommandFlag {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return match self {
            Self::Help => write!(f, "help"),
            Self::HelpShort => write!(f, "-h"),
            Self::HelpLong => write!(f, "--help"),
            Self::HelpQuestion => write!(f, "?"),
            Self::HelpQuestionShort => write!(f, "-?"),
            Self::HelpQuestionLong => write!(f, "--?"),
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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.color.set(&std::io::stdout());
        return write!(f, "Kaylang compiler, version {VERSION}");
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Help {
    pub executable_name: PathBuf,
    pub color: Color,
}

// IDEA(stefano): implement some checks to make sure that help messages don't go over 80 columns
impl Display for Help {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return write!(
            f,
            r"{Version}

{USAGE}: {executable_name} [{OPTIONS}] [{COMMAND}]

{OPTIONS}:
    {_c}, {__color} <{MODE}>
        Wether to display colored output, with <{MODE}>:
        - {auto} (default): only print colored output if supported
        - {always}: always print colored output, even if not supported
        - {never}: never print colored output

{COMMAND}s:
    {help}, {_h}, {__help}, {hq}, {_hq}, {__hq}            Display this message (default)
    {version}, {_v}, {__version}                  Display the compiler version

    {check}    <{FILE}>          [{VERBOSITY}]
        Check the source code for correctness

    {compile}  <{FILE}> [{OUTPUT}] [{VERBOSITY}]
        Compile the source code down to an executable

    {run}      <{FILE}> [{OUTPUT}] [{VERBOSITY}]
        Compile and run the generated executable

    {VERBOSITY}:
        {_q}, {__quiet}     Don't display any compilation information
        {_V}, {__verbose}   Display extra compilation information

    {OUTPUT}:
        {_o}, {__output} <{PATH}>
            Folder to populate with compilation artifacts (default: '.')",
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
            hq = CommandFlag::HelpQuestion,
            _hq = CommandFlag::HelpQuestionShort,
            __hq = CommandFlag::HelpQuestionLong,

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

    // IDEA(stefano): make help and version commands collide with other commands
    // i.e.: `kay run file.txt help` should raise an error
    fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
        Color::Auto.set(&std::io::stderr());

        let mut args_iter = args.iter().enumerate().peekable();
        let Some((_executable_name_index, executable_name)) = args_iter.next() else {
            return Err(Error::EmptyArgs)
        };

        let mut color_mode: Option<ColorMode> = None;
        let mut command_option: Option<(CommandFlag, Command)> = None;
        let mut errors = Vec::<(ErrorKind, usize)>::new();
        'args: while let Some((selected_flag_index, selected_flag)) = args_iter.next() {
            match selected_flag.as_str() {
                help_command @ ("help" | "-h" | "--help" | "?" | "-?" | "--?") => {
                    let help_flag = match help_command {
                        "help" => CommandFlag::Help,
                        "-h" => CommandFlag::HelpShort,
                        "--help" => CommandFlag::HelpLong,
                        "?" => CommandFlag::HelpQuestion,
                        "-?" => CommandFlag::HelpQuestionShort,
                        "--?" => CommandFlag::HelpQuestionLong,
                        _ => unreachable!(),
                    };

                    if let Some((previous_help_flag, Command::Help { .. } | Command::Version)) =
                        command_option
                    {
                        errors.push((ErrorKind::CommandAlreadySelected {
                                current: help_flag,
                                previous: previous_help_flag,
                            },
                            selected_flag_index,
                        ));
                        continue 'args;
                    }

                    command_option = Some((
                        help_flag,
                        Command::Help { executable_name: PathBuf::from(executable_name) },
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
                        errors.push((ErrorKind::CommandAlreadySelected {
                                current: version_flag,
                                previous: previous_version_flag,
                            },
                            selected_flag_index,
                        ));
                        continue 'args;
                    }

                    command_option = Some((version_flag, Command::Version));
                }
                "check" => {
                    let command_flag = CommandFlag::Check;

                    let Some((src_path_index, src_path_string)) = args_iter.next() else {
                        errors.push((
                            ErrorKind::MustBeFollowedByASourceFilePath(command_flag),
                            selected_flag_index,
                        ));
                        continue 'args;
                    };

                    let src_path = Path::new(src_path_string);
                    if !src_path.is_file() {
                        errors.push((ErrorKind::MustBeAFilePath, src_path_index));
                        continue 'args;
                    }

                    let verbosity = if let Some((_verbosity_flag_index, verbosity_flag)) = args_iter.peek() {
                        match verbosity_flag.as_str() {
                            "-q" | "--quiet" => {
                                _ = args_iter.next();
                                Verbosity::Quiet
                            }
                            "-V" | "--verbose" => {
                                _ = args_iter.next();
                                Verbosity::Verbose
                            }
                            _ => Verbosity::Normal
                        }
                    } else {
                        Verbosity::Normal
                    };

                    match &command_option {
                        None => {
                            let mode = Command::Check { src_path: src_path.to_path_buf(), verbosity };
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
                                errors.push((ErrorKind::CommandAlreadySelected {
                                        current: command_flag,
                                        previous: *previous_command_flag,
                                    },
                                    selected_flag_index,
                                ));
                                continue 'args;
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

                    let Some((src_path_index, src_path_string)) = args_iter.next() else {
                        errors.push((ErrorKind::MustBeFollowedByASourceFilePath(command_flag),
                            selected_flag_index,
                        ));
                        continue 'args;
                    };

                    let src_path = Path::new(src_path_string);
                    if !src_path.is_file() {
                        errors.push((ErrorKind::MustBeAFilePath,
                            src_path_index,
                        ));
                        continue 'args;
                    }

                    let out_path = 'out_path: {
                        if let Some((peeked_out_flag_index, out_flag)) = args_iter.peek() {
                            let out_flag_index = *peeked_out_flag_index;
                            let out_option = match out_flag.as_str() {
                                "-o" => {
                                    _ = args_iter.next();
                                    OutputFlag::Short
                                }
                                "--output" => {
                                    _ = args_iter.next();
                                    OutputFlag::Long
                                }
                                _ => break 'out_path None,
                            };

                            let Some((out_path_index, out_path_string)) = args_iter.next() else {
                                errors.push((ErrorKind::MustBeFollowedByDirectoryPath(out_option),
                                    out_flag_index,
                                ));
                                continue 'args;
                            };

                            let out_path_buf = Path::new(out_path_string);
                            if out_path_buf.is_file() {
                                errors.push((ErrorKind::MustBeADirectoryPath,
                                    out_path_index,
                                ));
                                continue 'args;
                            }

                            Some(out_path_buf.to_path_buf())
                        } else {
                            None
                        }
                    };

                    let verbosity = if let Some((_verbosity_flag_index, verbosity_flag)) = args_iter.peek() {
                        match verbosity_flag.as_str() {
                            "-q" | "--quiet" => {
                                _ = args_iter.next();
                                Verbosity::Quiet
                            }
                            "-V" | "--verbose" => {
                                _ = args_iter.next();
                                Verbosity::Verbose
                            }
                            _ => Verbosity::Normal
                        }
                    } else {
                        Verbosity::Normal
                    };

                    match &command_option {
                        None => {
                            let mode = match command_flag {
                                CommandFlag::Compile => Command::Compile {
                                    src_path: src_path.to_path_buf(),
                                    out_path, verbosity
                                },
                                CommandFlag::Run => Command::Run {
                                    src_path: src_path.to_path_buf(),
                                    out_path,
                                    verbosity
                                },
                                CommandFlag::Help
                                | CommandFlag::HelpShort
                                | CommandFlag::HelpLong
                                | CommandFlag::HelpQuestion
                                | CommandFlag::HelpQuestionShort
                                | CommandFlag::HelpQuestionLong
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
                                errors.push((ErrorKind::CommandAlreadySelected {
                                        current: command_flag,
                                        previous: *previous_command_flag,
                                    },
                                    selected_flag_index,
                                ));
                                continue 'args;
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

                    let Some((out_path_index, out_path_string)) = args_iter.next() else {
                        errors.push((ErrorKind::MustBeFollowedByDirectoryPath(flag),
                            selected_flag_index,
                        ));
                        continue 'args;
                    };

                    let out_path_buf = Path::new(out_path_string);
                    if !out_path_buf.is_dir() {
                        errors.push((ErrorKind::MustBeADirectoryPath,
                            out_path_index,
                        ));
                    }

                    errors.push((ErrorKind::StrayOutputDirectoryOption(flag),
                        selected_flag_index,
                    ));
                    continue 'args;
                }
                verbosity_flag @ ("-q" | "--quiet" | "-V" | "--verbose") => {
                    let flag = match verbosity_flag {
                        "-q" => VerbosityFlag::QuietShort,
                        "--quiet" => VerbosityFlag::QuietLong,
                        "-V" => VerbosityFlag::VerboseShort,
                        "--verbose" => VerbosityFlag::VerboseLong,
                        _ => unreachable!(),
                    };

                    errors.push((ErrorKind::StrayVerbosityOption(flag),
                        selected_flag_index,
                    ));
                    continue 'args;
                }
                color_flag @ ("-c" | "--color") => {
                    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                    #[repr(u8)]
                    enum ColorOption {
                        Auto = Color::Auto as u8,
                        Always = Color::Always as u8,
                        Never = Color::Never as u8,
                        Unknown,
                    }

                    let flag = match color_flag {
                        "-c" => ColorFlag::Short,
                        "--color" => ColorFlag::Long,
                        _ => unreachable!(),
                    };

                    let Some((selected_color_index, selected_color)) = args_iter.next() else {
                        errors.push((ErrorKind::MissingColorMode(flag),
                            selected_flag_index,
                        ));
                        continue 'args;
                    };

                    let color_option = match selected_color.as_str() {
                        "auto" => ColorOption::Auto,
                        "always" => ColorOption::Always,
                        "never" => ColorOption::Never,
                        _ => {
                            errors.push((ErrorKind::UnrecognizedColorMode,
                                selected_color_index,
                            ));
                            continue 'args;
                            #[expect(unreachable_code)]
                            ColorOption::Unknown
                        }
                    };

                    if let Some(ColorMode { flag: previous_flag, color: previous_color }) = color_mode {
                        errors.push((ErrorKind::ColorModeAlreadySelected {
                                current_flag: flag,
                                previous_flag,
                                previous_color,
                            },
                            selected_flag_index,
                        ));
                        continue 'args;
                    }

                    match color_option {
                        ColorOption::Auto
                        | ColorOption::Always
                        | ColorOption::Never => {
                            let selected_color_mode = ColorMode { flag, color: unsafe { core::mem::transmute(color_option) } };
                            color_mode = Some(selected_color_mode);
                        }
                        ColorOption::Unknown => {
                            // don't set the color mode
                        },
                    }
                }
                _ => {
                    errors.push((ErrorKind::Unrecognized,
                        selected_flag_index,
                    ));
                    continue 'args;
                }
            }
        }

        let color = match color_mode {
            Some(ColorMode { color, .. }) => color,
            None => Color::Auto,
        };
        color.set(&std::io::stderr());

        // IDEA(stefano): consider returning all the errors
        // Note: only returning the first error to make sure that any color flags in the middle are
        // properly parsed
        if let Some((kind, erroneous_arg_index)) = errors.first() {
            return Err(Error::FromArgs {
                kind: *kind,
                args, erroneous_arg_index:
                *erroneous_arg_index
            });
        }

        let command = match command_option {
            Some((_, command)) => command,
            None => Command::Help { executable_name: PathBuf::from(executable_name) },
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

// IDEA(stefano): remove redundant fields or redundant `erroneous_arg_index`
#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    MissingColorMode(ColorFlag),
    UnrecognizedColorMode,
    ColorModeAlreadySelected {
        current_flag: ColorFlag,
        previous_flag: ColorFlag,
        previous_color: Color,
    },

    CommandAlreadySelected {
        current: CommandFlag,
        previous: CommandFlag
    },

    // IDEA(stefano): add checks for existing file paths
    MustBeFollowedByASourceFilePath(CommandFlag),
    MustBeAFilePath,
    MustBeFollowedByDirectoryPath(OutputFlag),
    MustBeADirectoryPath,

    StrayVerbosityOption(VerbosityFlag),
    StrayOutputDirectoryOption(OutputFlag),

    Unrecognized,
}

#[derive(Debug)]
pub enum Error {
    EmptyArgs,
    FromArgs {
        kind: ErrorKind,
        args: Vec<String>,
        erroneous_arg_index: usize, // IDEA(stefano): inline into `ErrorKind` with custom names
    },
}

impl Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
                pointers_count = match erroneous_arg.len() {
                    0 => 1, // empty arguments will at least get one pointer
                    other => other,
                };

                match kind {
                    ErrorKind::MissingColorMode(flag) => {
                        _ = write!(error_message, "missing color mode for '{flag}'");
                        _ = write!(
                            error_cause_message,
                            "must be followed by '{auto}', '{always}' or '{never}'",
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
                    ErrorKind::ColorModeAlreadySelected {
                        current_flag,
                        previous_flag,
                        previous_color,
                    } => {
                        // starting the pointers at the flag and extending them up to the color mode
                        let erroneous_color = &args[*erroneous_arg_index + 1];
                        pointers_count += erroneous_color.len() + 1; // + 1 to account for the space between args

                        _ = write!(error_message, "repeated '{current_flag} {erroneous_color}' color mode");
                        _ = write!(
                            error_cause_message,
                            "cannot use '{current_flag} {erroneous_color}' because '{previous_flag} {previous_color}' was already selected"
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
                    ErrorKind::MustBeAFilePath => {
                        let path = &args[*erroneous_arg_index];
                        _ = write!(error_message, "invalid '{path}' path");
                        _ = write!(error_cause_message, "'{path}' must be a file path");
                    }
                    ErrorKind::MustBeFollowedByDirectoryPath(option) => {
                        _ = write!(error_message, "invalid '{option}' option");
                        _ = write!(
                            error_cause_message,
                            "'{option}' must be followed by a directory path"
                        );
                    }
                    ErrorKind::MustBeADirectoryPath => {
                        let path = &args[*erroneous_arg_index];
                        _ = write!(error_message, "invalid '{path}' path");
                        _ = write!(error_cause_message, "'{path}' must be a directory path");
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

        let error = MsgWithCauseUnderText {
            kind: &ERROR,
            message: &error_message,
            cause: &error_cause_message,
            line_text: &args_text,
            pointers_offset: pointers_offset as column32,
            pointers_count: pointers_count as column32,
        };
        return write!(f, "{error}");
    }
}

#[expect(clippy::missing_trait_methods, reason = "using default implementations")]
impl core::error::Error for Error {}
