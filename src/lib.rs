// IDEA(stefano): warn on invalid "-output" (correct: "--output") or "--o" (correct: "-o") or similar

#![warn(clippy::print_stdout, clippy::print_stderr)]

#[cfg(not(target_pointer_width = "64"))]
compile_error!("can only compile for 64bit machines for now");

pub mod back_end;
pub mod color;
pub mod error;
pub mod front_end;

use back_to_front::offset32;
use color::{ansi_flag, AnsiFlag, Bg, Colored, Fg};
use core::fmt::{Display, Write as _};
use error::MsgWithCauseUnderText;
use std::{
    io::IsTerminal,
    path::{Path, PathBuf},
    time::Instant,
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

macro_rules! messages {
    (
        $padding_storage:ident $padding:ident: $padding_typ:ty = $padding_fn:ident;
        $($visibility:vis $step_name:ident = $text:literal, $fg:ident, $bg:ident, $flags:ident $(,)?;)+
    ) => {
        $padding_storage $padding: $padding_typ = $padding_fn(&[$($step_name.text,)+]);
        messages!($($visibility $step_name = $text, $fg, $bg, $flags;)+);
    };

    ($($visibility:vis $step_name:ident = $text:literal, $fg:ident, $bg:ident, $flags:ident $(,)?;)+) => {
        $($visibility static $step_name: Colored<'_, str> = Colored { text: $text, fg: $fg, bg: $bg, flags: $flags };)+
    };
}

// main compilation steps (displayed when verbosity level is normal or verbose)
const STEP_FG: Fg = Fg::LightGreen;
const STEP_BG: Bg = Bg::Default;
const STEP_FLAGS: ansi_flag = AnsiFlag::Bold as ansi_flag;
const STEP_INDENT: usize = 0;
messages!(
    // TODO(stefano): change to const when upgrading rust version
    static STEP_PADDING: usize = max_text_len;
    pub CHECKING  = "Checking",  STEP_FG, STEP_BG, STEP_FLAGS;
    pub COMPILING = "Compiling", STEP_FG, STEP_BG, STEP_FLAGS;
    pub RUNNING   = "Running",   STEP_FG, STEP_BG, STEP_FLAGS;
    pub DONE      = "Done",      STEP_FG, STEP_BG, STEP_FLAGS;
);

// sub compilation steps (displayed when verbosity lever is verbose)
const SUBSTEP_FG: Fg = Fg::LightBlue;
const SUBSTEP_BG: Bg = Bg::Default;
const SUBSTEP_FLAGS: ansi_flag = AnsiFlag::Bold as ansi_flag;
const SUBSTEP_INDENT: usize = STEP_INDENT + 4;
messages!(
    // TODO(stefano): change to const when upgrading rust version
    static SUBSTEP_PADDING: usize = max_text_len;
    pub LOADING_SOURCE      = "Loading Source",      SUBSTEP_FG, SUBSTEP_BG, SUBSTEP_FLAGS;
    pub TOKENIZATION        = "Tokenizing",          SUBSTEP_FG, SUBSTEP_BG, SUBSTEP_FLAGS;
    pub PARSING_SYNTAX_TREE = "Parsing Syntax Tree", SUBSTEP_FG, SUBSTEP_BG, SUBSTEP_FLAGS;
    pub TYPE_CHECKING       = "Type-checking",       SUBSTEP_FG, SUBSTEP_BG, SUBSTEP_FLAGS;
    pub PARSING_AST         = "Parsing Ast",         SUBSTEP_FG, SUBSTEP_BG, SUBSTEP_FLAGS;
    pub GENERATING_ASM      = "Generating asm",      SUBSTEP_FG, SUBSTEP_BG, SUBSTEP_FLAGS;
    pub ASSEMBLING          = "Assembling",          SUBSTEP_FG, SUBSTEP_BG, SUBSTEP_FLAGS;
    pub LINKING             = "Linking",             SUBSTEP_FG, SUBSTEP_BG, SUBSTEP_FLAGS;
    pub SUBSTEP_DONE        = "Done",                SUBSTEP_FG, SUBSTEP_BG, SUBSTEP_FLAGS;
);

// errors
const ERR_FG: Fg = Fg::LightRed;
const ERR_BG: Bg = Bg::Default;
const ERR_FLAGS: ansi_flag = AnsiFlag::Bold as ansi_flag;

const BAR_FG: Fg = Fg::LightBlue;
const BAR_BG: Bg = Bg::Default;
const BAR_FLAGS: ansi_flag = AnsiFlag::Bold as ansi_flag;

messages!(
    pub(crate) ERROR = "Error", ERR_FG, ERR_BG, ERR_FLAGS;
    pub(crate) CAUSE = "Cause", ERR_FG, ERR_BG, ERR_FLAGS;
    pub(crate) AT    = "at",    ERR_FG, ERR_BG, ERR_FLAGS;
    pub(crate) BAR   = "|",     BAR_FG, BAR_BG, BAR_FLAGS;
);
messages!(
    pub COULD_NOT_WRITE_COMPILED_CODE = "Could not write compile code", ERR_FG, ERR_BG, ERR_FLAGS;
    pub COULD_NOT_RUN_ASSEMBLER       = "Could not run assembler",      ERR_FG, ERR_BG, ERR_FLAGS;
    pub COULD_NOT_RUN_LINKER          = "Could not run linker",         ERR_FG, ERR_BG, ERR_FLAGS;
    pub COULD_NOT_RUN_EXECUTABLE      = "Could not run executable",     ERR_FG, ERR_BG, ERR_FLAGS;
    pub ASSEMBLING_ERROR              = "Assembling Error",             ERR_FG, ERR_BG, ERR_FLAGS;
    pub LINKING_ERROR                 = "Linking Error",                ERR_FG, ERR_BG, ERR_FLAGS;
);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
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
            text: &format!("{:.06}s", self.start.elapsed().as_secs_f32()),
            fg: Fg::White,
            bg: Bg::Default,
            flags: AnsiFlag::Default as u8,
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
    pub fn step_with_verbosity(
        self,
        text: &dyn Display,
        output: Option<&Path>,
        verbosity: Verbosity,
    ) {
        if let Verbosity::Normal | Verbosity::Verbose = verbosity {
            self.done(text, output, STEP_INDENT + STEP_PADDING);
        }
    }

    #[inline(always)]
    pub fn sub_step(self, text: &dyn Display, output: Option<&Path>) {
        self.done(text, output, SUBSTEP_INDENT + SUBSTEP_PADDING);
    }

    #[inline(always)]
    pub fn sub_step_with_verbosity(
        self,
        text: &dyn Display,
        output: Option<&Path>,
        verbosity: Verbosity,
    ) {
        if let Verbosity::Verbose = verbosity {
            self.done(text, output, SUBSTEP_INDENT + SUBSTEP_PADDING);
        }
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum FlagPrefix {
    Empty    = 0b0000_0000,
    Dash     = 0b0100_0000,
    DashDash = 0b1000_0000,
    Slash    = 0b1100_0000,
}

impl FlagPrefix {
    pub const MASK: u8 = 0b1100_0000;
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum FlagLen {
    Long  = 0b0000_0000,
    Short = 0b0010_0000,
}

impl FlagLen {
    pub const MASK: u8 = 0b0010_0000;
}

macro_rules! flag {
    ($base:expr, $prefix:ident, $len:ident) => {
        $base | FlagPrefix::$prefix as u8 | FlagLen::$len as u8
    };
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum ColorFlag {
    Long       = flag!(0b0000_0000, DashDash, Long),
    LongSlash  = flag!(0b0000_0000, Slash,    Long),
    Short      = flag!(0b0000_0000, Dash,     Short),
    ShortSlash = flag!(0b0000_0000, Slash,    Short),
}

impl Display for ColorFlag {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Long       => write!(f, "--color"),
            Self::LongSlash  => write!(f, "/color"),
            Self::Short      => write!(f, "-c"),
            Self::ShortSlash => write!(f, "/c"),
        };
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum Color {
    #[default]
    Auto   = 0b0000_0000,
    Always = 0b0000_0001,
    Never  = 0b0000_0010,
}

impl Display for Color {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Auto   => write!(f, "auto"),
            Self::Always => write!(f, "always"),
            Self::Never  => write!(f, "never"),
        };
    }
}

impl Color {
    #[inline]
    pub fn set<I: IsTerminal>(self, sink: &I) {
        match self {
            Self::Auto => Self::set_color_auto(sink),
            Self::Always => Self::set_color_always(),
            Self::Never => Self::set_color_never(),
        }
    }

    #[inline(always)]
    pub fn set_color_auto<S: IsTerminal>(sink: &S) {
        use crate::color::{print, print_color, print_no_color};
        unsafe {
            print = if sink.is_terminal() { print_color } else { print_no_color };
        }
    }

    #[inline(always)]
    pub fn set_color_always() {
        use crate::color::{print, print_color};
        unsafe {
            print = print_color;
        }
    }

    #[inline(always)]
    pub fn set_color_never() {
        use crate::color::{print, print_no_color};
        unsafe {
            print = print_no_color;
        }
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum CommandFlag {
    Help                   = flag!(Self::HELP,          Empty,    Long),
    HelpLong               = flag!(Self::HELP,          DashDash, Long),
    HelpLongSlash          = flag!(Self::HELP,          Slash,    Long),
    HelpShort              = flag!(Self::HELP,          Dash,     Short),
    HelpShortSlash         = flag!(Self::HELP,          Slash,    Short),

    HelpQuestion           = flag!(Self::HELP_QUESTION, Empty,    Long),
    HelpQuestionLong       = flag!(Self::HELP_QUESTION, DashDash, Long),
    HelpQuestionShort      = flag!(Self::HELP_QUESTION, Dash,     Short),
    HelpQuestionShortSlash = flag!(Self::HELP_QUESTION, Slash,    Short),

    Version                = flag!(Self::VERSION,       Empty,    Long),
    VersionLong            = flag!(Self::VERSION,       DashDash, Long),
    VersionLongSlash       = flag!(Self::VERSION,       Slash,    Long),
    VersionShort           = flag!(Self::VERSION,       Dash,     Short),
    VersionShortSlash      = flag!(Self::VERSION,       Slash,    Short),

    Check                  = flag!(Self::CHECK,         Empty,    Long),
    Compile                = flag!(Self::COMPILE,       Empty,    Long),
    CompileAsm             = flag!(Self::COMPILE_ASM,   Empty,    Long),
    CompileObj             = flag!(Self::COMPILE_OBJ,   Empty,    Long),
    Run                    = flag!(Self::RUN,           Empty,    Long),
    RunAsm                 = flag!(Self::RUN_ASM,       Empty,    Long),
    RunObj                 = flag!(Self::RUN_OBJ,       Empty,    Long),
}

#[rustfmt::skip]
impl CommandFlag {
    const HELP: u8          = 0b0000_0000;
    const HELP_QUESTION: u8 = 0b0000_0001;
    const VERSION: u8       = 0b0000_0010;
    const CHECK: u8         = 0b0000_1000;
    const COMPILE: u8       = 0b0000_1001;
    const COMPILE_ASM: u8   = 0b0000_1010;
    const COMPILE_OBJ: u8   = 0b0000_1011;
    const RUN: u8           = 0b0000_1101;
    const RUN_ASM: u8       = 0b0000_1110;
    const RUN_OBJ: u8       = 0b0000_1111;
}

impl Display for CommandFlag {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Help                   => write!(f, "help"),
            Self::HelpLong               => write!(f, "--help"),
            Self::HelpLongSlash          => write!(f, "/help"),
            Self::HelpShort              => write!(f, "-h"),
            Self::HelpShortSlash         => write!(f, "/h"),

            Self::HelpQuestion           => write!(f, "?"),
            Self::HelpQuestionLong       => write!(f, "--?"),
            Self::HelpQuestionShort      => write!(f, "-?"),
            Self::HelpQuestionShortSlash => write!(f, "/?"),

            Self::Version                => write!(f, "version"),
            Self::VersionLong            => write!(f, "--version"),
            Self::VersionLongSlash       => write!(f, "/version"),
            Self::VersionShort           => write!(f, "-v"),
            Self::VersionShortSlash      => write!(f, "/v"),

            Self::Check                  => write!(f, "check"),
            Self::Compile                => write!(f, "compile"),
            Self::CompileAsm             => write!(f, "compile-asm"),
            Self::CompileObj             => write!(f, "compile-obj"),
            Self::Run                    => write!(f, "run"),
            Self::RunAsm                 => write!(f, "run-asm"),
            Self::RunObj                 => write!(f, "run-obj"),
        };
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub enum Command<'args> {
    #[default]
    Help,
    Version,

    // TODO(stefano): make composable (repr(u8)) for easy construction
    Check { src_path: &'args Path, verbosity: Verbosity },
    Compile { src_path: &'args Path, verbosity: Verbosity, out_path: &'args Path, language: Language },
    Run { src_path: &'args Path, verbosity: Verbosity, out_path: &'args Path, language: Language },
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum Language {
    #[default]
    Kay = 0b0000_0000,
    Asm = 0b0000_0001,
    Obj = 0b0000_0010,
}

impl Display for Language {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Kay       => write!(f, "kay"),
            Self::Asm       => write!(f, "asm"),
            Self::Obj       => write!(f, "obj"),
        };
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum OutputFlag {
    Long       = flag!(0b0000_0000, DashDash, Long),
    LongSlash  = flag!(0b0000_0000, Slash,    Long),
    Short      = flag!(0b0000_0000, Dash,     Short),
    ShortSlash = flag!(0b0000_0000, Slash,    Short),
}

impl Display for OutputFlag {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Long       => write!(f, "--output"),
            Self::LongSlash  => write!(f, "/output"),
            Self::Short      => write!(f, "-o"),
            Self::ShortSlash => write!(f, "/o"),
        };
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum VerbosityFlag {
    QuietLong         = flag!(Verbosity::Quiet as u8, DashDash, Long),
    QuietLongSlash    = flag!(Verbosity::Quiet as u8, Slash,    Long),
    QuietShort        = flag!(Verbosity::Quiet as u8, Dash,     Short),
    QuietShortSlash   = flag!(Verbosity::Quiet as u8, Slash,    Short),

    VerboseLong       = flag!(Verbosity::Verbose as u8, DashDash, Long),
    VerboseLongSlash  = flag!(Verbosity::Verbose as u8, Slash,    Long),
    VerboseShort      = flag!(Verbosity::Verbose as u8, Dash,     Short),
    VerboseShortSlash = flag!(Verbosity::Verbose as u8, Slash,    Short),
}

impl Display for VerbosityFlag {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::QuietLong         => write!(f, "--quiet"),
            Self::QuietLongSlash    => write!(f, "/quiet"),
            Self::QuietShort        => write!(f, "-q"),
            Self::QuietShortSlash   => write!(f, "/q"),

            Self::VerboseLong       => write!(f, "--Verbose"),
            Self::VerboseLongSlash  => write!(f, "/Verbose"),
            Self::VerboseShort      => write!(f, "-V"),
            Self::VerboseShortSlash => write!(f, "/V"),
        };
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum Verbosity {
    #[default]
    Normal  = 0b0000_0000,
    Quiet   = 0b0000_0001,
    Verbose = 0b0000_0010,
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
#[repr(transparent)]
pub struct Version {
    pub color: Color,
}

impl Display for Version {
    #[expect(non_upper_case_globals)]
    #[rustfmt::skip]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        const fg: Fg = Fg::White;
        const bg: Bg = Bg::Default;
        const flags: ansi_flag = AnsiFlag::Bold as ansi_flag;

        self.color.set(&std::io::stdout());
        return write!(f,
            "Kaylang compiler, version {VERSION}",
            VERSION = Colored { text: env!("CARGO_PKG_VERSION"), fg, bg, flags },
        );
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Help {
    pub color: Color,
    // IDEA(stefano): make this a Cow<'args, Path>
    pub executable_name: PathBuf,
}

impl Help {
    #[must_use]
    #[inline(always)]
    pub fn default_executable_name() -> &'static Path {
        return Path::new("kay");
    }
}

impl Default for Help {
    #[must_use]
    #[inline]
    fn default() -> Self {
        return Self {
            color: Color::Auto,
            executable_name: Self::default_executable_name().to_owned(),
        };
    }
}

impl Display for Help {
    #[expect(non_upper_case_globals)]
    #[rustfmt::skip]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        const fg_value:  Fg = Fg::White;
        const fg_option: Fg = Fg::Green;
        const fg_flag:   Fg = Fg::Blue;
        const bg:        Bg = Bg::Default;
        const flags_value:  ansi_flag = AnsiFlag::Bold as ansi_flag;
        const flags_option: ansi_flag = AnsiFlag::Bold as ansi_flag;
        const flags_flag:   ansi_flag = AnsiFlag::Bold as ansi_flag;

        // IDEA(stefano): add "--file"/"/file"/"-f"/"/f" option to specify a file with a name that
        // may collide with a flag, i.e.: what if i want to compile a file named "--output"
        return write!(
            f,
            r"{Version}

{Usage}: {executable_name} [{Options}] [{Command}]

[{Options}]:
    {__color}, {Scolor}, {_c}, {Sc} <{color}>
        (supports '{s}-{color}', '{s}={color}' or '{s}:{color}' variations: '{_c}={auto}')

    <{color}>:
        {auto} (default)    only print colored output if supported
        {always}            always print colored output, even if not supported
        {never}             never print colored output

[{Command}]s:
    {help},    {__help},    {Shelp},    {_h}, {Sh}, {hq}, {__hq}, {_hq}, {Shq}
        Display this message, and ignore any other command
        (also selected if no other arguments are provided)

    {version}, {__version}, {Sversion}, {_v}, {Sv}
        Display the compiler version

    {check}                             <{file}> [{Check_Options}]
        Check kay <{file}> for correctness

    {compile}, {compile}-{asm}, {compile}-{obj} <{file}> <{Output}> [{Compile_Options}]
        Compile kay/assembly/object <{file}> down to an executable

    {run},     {run}-{asm},     {run}-{obj}     <{file}> <{Output}> [{Run_Options}]
        Compile kay/assembly/object <{file}> and run the generated executable

    [{Check_Options}], [{Compile_Options}], [{Run_Options}]:
        {__quiet},   {Squiet},   {_q}, {Sq}
            Don't display any compilation information

        {__Verbose}, {SVerbose}, {_V}, {SV}
            Display extra compilation information

    <{Output}>:
        {__output}, {Soutput}, {_o}, {So} <{path}>
            (supports '{s}={path}' or `{s}:{path}' variations: '{_o}={out}')

        <{path}>:
            Folder to populate with compilation artifacts",

            Version         = Version { color: self.color },
            executable_name = self.executable_name.display(),
            Usage           = Colored { text: "Usage",           fg: fg_value,  bg, flags: flags_value },
            Options         = Colored { text: "Options",         fg: fg_option, bg, flags: flags_option },
            Command         = Colored { text: "Command",         fg: fg_option, bg, flags: flags_option },
            color           = Colored { text: "color",           fg: fg_value,  bg, flags: flags_value },
            file            = Colored { text: "file",            fg: fg_value,  bg, flags: flags_value },
            path            = Colored { text: "path",            fg: fg_value,  bg, flags: flags_value },
            Check_Options   = Colored { text: "Check Options",   fg: fg_option, bg, flags: flags_option },
            Compile_Options = Colored { text: "Compile Options", fg: fg_option, bg, flags: flags_option },
            Run_Options     = Colored { text: "Run Options",     fg: fg_option, bg, flags: flags_option },
            Output          = Colored { text: "Output",          fg: fg_option, bg, flags: flags_option },

            s   = Colored { text: "*", fg: fg_flag, bg, flags: flags_flag },
            out = Colored { text: "out", fg: fg_value, bg, flags: flags_value },

            __color = Colored { text: &ColorFlag::Long,       fg: fg_flag, bg, flags: flags_flag },
            Scolor  = Colored { text: &ColorFlag::LongSlash,  fg: fg_flag, bg, flags: flags_flag },
            _c      = Colored { text: &ColorFlag::Short,      fg: fg_flag, bg, flags: flags_flag },
            Sc      = Colored { text: &ColorFlag::ShortSlash, fg: fg_flag, bg, flags: flags_flag },
            auto    = Colored { text: &Color::Auto,           fg: fg_flag, bg, flags: flags_flag },
            always  = Colored { text: &Color::Always,         fg: fg_flag, bg, flags: flags_flag },
            never   = Colored { text: &Color::Never,          fg: fg_flag, bg, flags: flags_flag },

            help   = Colored { text: &CommandFlag::Help,                   fg: fg_flag, bg, flags: flags_flag },
            __help = Colored { text: &CommandFlag::HelpLong,               fg: fg_flag, bg, flags: flags_flag },
            Shelp  = Colored { text: &CommandFlag::HelpLongSlash,          fg: fg_flag, bg, flags: flags_flag },
            _h     = Colored { text: &CommandFlag::HelpShort,              fg: fg_flag, bg, flags: flags_flag },
            Sh     = Colored { text: &CommandFlag::HelpShortSlash,         fg: fg_flag, bg, flags: flags_flag },
            hq     = Colored { text: &CommandFlag::HelpQuestion,           fg: fg_flag, bg, flags: flags_flag },
            __hq   = Colored { text: &CommandFlag::HelpQuestionLong,       fg: fg_flag, bg, flags: flags_flag },
            _hq    = Colored { text: &CommandFlag::HelpQuestionShort,      fg: fg_flag, bg, flags: flags_flag },
            Shq    = Colored { text: &CommandFlag::HelpQuestionShortSlash, fg: fg_flag, bg, flags: flags_flag },

            version   = Colored { text: &CommandFlag::Version,           fg: fg_flag, bg, flags: flags_flag },
            __version = Colored { text: &CommandFlag::VersionLong,       fg: fg_flag, bg, flags: flags_flag },
            Sversion  = Colored { text: &CommandFlag::VersionLongSlash,  fg: fg_flag, bg, flags: flags_flag },
            _v        = Colored { text: &CommandFlag::VersionShort,      fg: fg_flag, bg, flags: flags_flag },
            Sv        = Colored { text: &CommandFlag::VersionShortSlash, fg: fg_flag, bg, flags: flags_flag },

            check   = Colored { text: &CommandFlag::Check,   fg: fg_flag, bg, flags: flags_flag },
            compile = Colored { text: &CommandFlag::Compile, fg: fg_flag, bg, flags: flags_flag },
            run     = Colored { text: &CommandFlag::Run,     fg: fg_flag, bg, flags: flags_flag },

            asm = Colored { text: &Language::Asm, fg: fg_flag, bg, flags: flags_flag },
            obj = Colored { text: &Language::Obj, fg: fg_flag, bg, flags: flags_flag },

            __output = Colored { text: &OutputFlag::Long,       fg: fg_flag, bg, flags: flags_flag },
            Soutput  = Colored { text: &OutputFlag::LongSlash,  fg: fg_flag, bg, flags: flags_flag },
            _o       = Colored { text: &OutputFlag::Short,      fg: fg_flag, bg, flags: flags_flag },
            So       = Colored { text: &OutputFlag::ShortSlash, fg: fg_flag, bg, flags: flags_flag },

            __quiet   = Colored { text: &VerbosityFlag::QuietLong,         fg: fg_flag, bg, flags: flags_flag },
            Squiet    = Colored { text: &VerbosityFlag::QuietLongSlash,    fg: fg_flag, bg, flags: flags_flag },
            _q        = Colored { text: &VerbosityFlag::QuietShort,        fg: fg_flag, bg, flags: flags_flag },
            Sq        = Colored { text: &VerbosityFlag::QuietShortSlash,   fg: fg_flag, bg, flags: flags_flag },
            __Verbose = Colored { text: &VerbosityFlag::VerboseLong,       fg: fg_flag, bg, flags: flags_flag },
            SVerbose  = Colored { text: &VerbosityFlag::VerboseLongSlash,  fg: fg_flag, bg, flags: flags_flag },
            _V        = Colored { text: &VerbosityFlag::VerboseShort,      fg: fg_flag, bg, flags: flags_flag },
            SV        = Colored { text: &VerbosityFlag::VerboseShortSlash, fg: fg_flag, bg, flags: flags_flag },
        );
    }
}

#[must_use]
#[derive(Debug)]
enum ArgResult<P> {
    Ok(P),
    Err,
    Unrecognized,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Args<'args> {
    pub command: Result<Command<'args>, Vec<Error>>,
    pub color: Color,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct SrcPath<'args> {
    path: &'args Path,
    arg_index: usize,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct OutPath<'args> {
    path: &'args Path,
    arg_index: usize,
    start_of_path_character_index: u8,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArgsParser<'args, S: AsRef<str>> {
    args: &'args [S],
    arg_index: usize,

    color: Color,
    verbosity: Verbosity,
    command_flag: Option<(CommandFlag, usize)>,
    src_paths: Vec<SrcPath<'args>>,
    out_paths: Vec<OutPath<'args>>,
    errors: Vec<Error>,
}

// IDEA(stefano): split into ArgsTokenizer and ArgsParser
impl<'args, S: AsRef<str>> ArgsParser<'args, S> {
    // IDEA(stefano): make help and version commands collide with other commands
    // i.e.: `kay run file.txt help` should raise an error
    // i.e.: `kay run file.txt --help` should not raise an error
    #[must_use]
    pub fn parse(args: &'args [S]) -> Args<'args> {
        let mut parser = Self {
            args,
            arg_index: 0,

            color: Color::Auto,
            verbosity: Verbosity::Normal,
            command_flag: None,
            src_paths: Vec::new(),
            out_paths: Vec::new(),
            errors: Vec::new(),
        };

        while let Some(raw_arg) = parser.get_current_arg() {
            let current_flag_index = parser.arg_index;
            if raw_arg.len() == 0 {
                parser.errors.push(Error { kind: ErrorKind::Empty, arg_index: current_flag_index });
                parser.arg_index += 1;
                continue;
            }

            let (prefix, arg) = Self::split_prefix(raw_arg);

            match parser.parse_color_flag(arg, prefix) {
                ArgResult::Ok(color) => {
                    parser.color = color;
                    continue;
                },
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            match parser.parse_verbosity_flag(arg, prefix) {
                ArgResult::Ok(verbosity) => {
                    parser.verbosity = verbosity;
                    continue;
                },
                ArgResult::Err => unreachable!(),
                ArgResult::Unrecognized => {},
            }

            match parser.parse_command(arg, prefix, CommandFlag::Help) {
                ArgResult::Ok(command_flag) =>  {
                    parser.set_command(command_flag, current_flag_index);
                    continue;
                },
                ArgResult::Err => unreachable!(),
                ArgResult::Unrecognized => {},
            }

            match parser.parse_command(arg, prefix, CommandFlag::Version) {
                ArgResult::Ok(command_flag) => {
                    parser.set_command(command_flag, current_flag_index);
                    continue;
                },
                ArgResult::Err => unreachable!(),
                ArgResult::Unrecognized => {},
            }

            match parser.parse_command(arg, prefix, CommandFlag::Check) {
                ArgResult::Ok(command_flag) => {
                    parser.set_command(command_flag, current_flag_index);
                    continue;
                },
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            match parser.parse_command(arg, prefix, CommandFlag::Compile) {
                ArgResult::Ok(command_flag) => {
                    parser.set_command(command_flag, current_flag_index);
                    continue;
                },
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            match parser.parse_command(arg, prefix, CommandFlag::Run) {
                ArgResult::Ok(command_flag) => {
                    parser.set_command(command_flag, current_flag_index);
                    continue;
                },
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            match parser.parse_out_path(arg, prefix) {
                ArgResult::Ok(out_path) => {
                    parser.out_paths.push(out_path);
                    continue;
                },
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            match prefix {
                FlagPrefix::Dash | FlagPrefix::DashDash | FlagPrefix::Slash => {
                    parser.errors.push(Error {
                        kind: ErrorKind::Unrecognized,
                        arg_index: current_flag_index,
                    });
                    parser.arg_index += 1;
                    continue;
                }
                FlagPrefix::Empty => {},
            }

            let src_path = parser.parse_src_path(arg);
            parser.src_paths.push(src_path);
        }

        let command = parser.get_command_and_report_errors();

        if parser.errors.len() != 0 {
            return Args { color: parser.color, command: Err(parser.errors) };
        }
        let Ok(command_ok) = command else {
            return Args { color: parser.color, command: Err(parser.errors) };
        };
        return Args { color: parser.color, command: Ok(command_ok) };
    }

    #[must_use]
    fn get_current_arg(&self) -> Option<&'args str> {
        if self.arg_index >= self.args.len() {
            return None;
        }
        let arg = &self.args[self.arg_index];
        let arg_str = arg.as_ref();
        return Some(arg_str);
    }

    // TODO(stefano): use the function from `back-to-front`
    #[expect(clippy::single_call_fn)]
    #[must_use]
    fn split_prefix(arg: &str) -> (FlagPrefix, &str) {
        let arg_characters = arg.as_bytes();
        let (prefix, prefix_len) = match arg_characters.get(0) {
            Some(b'/') => (FlagPrefix::Slash, 1),
            Some(b'-') => match arg_characters.get(1) {
                Some(b'-') => (FlagPrefix::DashDash, 2),
                Some(_) | None => (FlagPrefix::Dash, 1),
            },
            Some(_) | None => (FlagPrefix::Empty, 0),
        };

        let argument = &arg[prefix_len..];
        return (prefix, argument);
    }
}

impl<'args, S: AsRef<str>> ArgsParser<'args, S> {
    fn parse_color_flag(
        &mut self,
        arg: &'args str,
        prefix: FlagPrefix,
    ) -> ArgResult<Color> {
        use FlagPrefix::{Dash, DashDash, Empty, Slash};
        const COLOR_LONG: &str = "color";
        const COLOR_SHORT: &str = "c";

        let (color_flag, separator_index, prefix_len) = if arg.starts_with(COLOR_LONG) {
            let (flag, prefix_len) = match prefix {
                DashDash => (ColorFlag::Long, 2),
                Slash => (ColorFlag::LongSlash, 1),
                Empty | Dash => return ArgResult::Unrecognized,
            };
            (flag, COLOR_LONG.len(), prefix_len)
        } else if arg.starts_with(COLOR_SHORT) {
            let (flag, prefix_len) = match prefix {
                Dash => (ColorFlag::Short, 1),
                Slash => (ColorFlag::ShortSlash, 1),
                Empty | DashDash => return ArgResult::Unrecognized,
            };
            (flag, COLOR_SHORT.len(), prefix_len)
        } else {
            return ArgResult::Unrecognized;
        };
        let color_flag_index = self.arg_index;

        let (color_str, color_mode_index, start_of_color_index) = 'color_str: {
            let Some(separator) = arg.as_bytes().get(separator_index) else {
                self.arg_index += 1;
                let Some(color_str) = self.get_current_arg() else {
                    self.errors.push(Error {
                        kind: ErrorKind::MustBeFollowedByColorMode(color_flag),
                        arg_index: color_flag_index,
                    });
                    return ArgResult::Err;
                };

                let color_mode_index = self.arg_index;
                break 'color_str (color_str, color_mode_index, 0);
            };

            let start_of_color_index = match separator {
                b'-' | b'=' | b':' => separator_index + 1,
                _ => return ArgResult::Unrecognized,
            };
            #[expect(clippy::cast_possible_truncation)]
            (
                &arg[start_of_color_index..],
                color_flag_index,
                (start_of_color_index + prefix_len) as u8,
            )
        };
        self.arg_index += 1;

        let color = match color_str {
            "auto" => Color::Auto,
            "always" => Color::Always,
            "never" => Color::Never,
            "" => {
                self.errors.push(Error {
                    kind: ErrorKind::MissingColorMode(color_flag),
                    arg_index: color_flag_index,
                });
                return ArgResult::Err;
            }
            _ => {
                self.errors.push(Error {
                    kind: ErrorKind::UnrecognizedColorMode { start_of_color_index },
                    arg_index: color_mode_index,
                });
                return ArgResult::Err;
            },
        };

        return ArgResult::Ok(color);
    }

    fn parse_verbosity_flag(
        &mut self,
        arg: &'args str,
        prefix: FlagPrefix,
    ) -> ArgResult<Verbosity> {
        use FlagPrefix::{Dash, DashDash, Empty, Slash};
        use Verbosity::{Quiet, Verbose};
        let verbosity = match arg {
            "quiet" => match prefix {
                DashDash | Slash => Quiet,
                Empty | Dash => return ArgResult::Unrecognized,
            },
            "q" => match prefix {
                Dash | Slash => Quiet,
                Empty | DashDash => return ArgResult::Unrecognized,
            },
            "Verbose" => match prefix {
                DashDash | Slash => Verbose,
                Empty | Dash => return ArgResult::Unrecognized,
            },
            "V" => match prefix {
                Dash | Slash => Verbose,
                Empty | DashDash => return ArgResult::Unrecognized,
            },
            _ => return ArgResult::Unrecognized,
        };
        self.arg_index += 1;

        return ArgResult::Ok(verbosity);
    }

    fn parse_command(
        &mut self,
        arg: &'args str,
        prefix: FlagPrefix,
        command_flag: CommandFlag,
    ) -> ArgResult<CommandFlag> {
        use CommandFlag::{
            Help, HelpLong, HelpLongSlash,
            HelpShort, HelpShortSlash,
            HelpQuestion, HelpQuestionLong, HelpQuestionShort, HelpQuestionShortSlash,
            Version, VersionLong, VersionLongSlash,
            VersionShort, VersionShortSlash,
            Check,
            Compile, CompileAsm, CompileObj,
            Run, RunAsm, RunObj,
        };
        use FlagPrefix::{Dash, DashDash, Empty, Slash};

        match command_flag {
            Help | HelpLong | HelpLongSlash
            | HelpShort | HelpShortSlash
            | HelpQuestion | HelpQuestionLong | HelpQuestionShort | HelpQuestionShortSlash => {
                let help_flag = match arg {
                    "help" => match prefix {
                        Empty => Help,
                        DashDash => HelpLong,
                        Slash => HelpLongSlash,
                        Dash => return ArgResult::Unrecognized,
                    },
                    "h" => match prefix {
                        Dash => HelpShort,
                        Slash => HelpShortSlash,
                        Empty | DashDash => return ArgResult::Unrecognized,
                    },
                    "?" => match prefix {
                        Empty => HelpQuestion,
                        DashDash => HelpQuestionLong,
                        Dash => HelpQuestionShort,
                        Slash => HelpQuestionShortSlash,
                    },
                    _ => return ArgResult::Unrecognized,
                };
                self.arg_index += 1;

                return ArgResult::Ok(help_flag);
            },

            Version | VersionLong | VersionLongSlash
            | VersionShort | VersionShortSlash => {
                let version_flag = match arg {
                    "version" => match prefix {
                        Empty => Version,
                        DashDash => VersionLong,
                        Slash => VersionLongSlash,
                        Dash => return ArgResult::Unrecognized,
                    },
                    "v" => match prefix {
                        Dash => VersionShort,
                        Slash => VersionShortSlash,
                        Empty | DashDash => return ArgResult::Unrecognized,
                    },
                    _ => return ArgResult::Unrecognized,
                };
                self.arg_index += 1;

                return ArgResult::Ok(version_flag);
            }

            Check => {
                const CHECK_LONG: &str = "check";

                if arg != CHECK_LONG {
                    return ArgResult::Unrecognized;
                }
                match prefix {
                    Empty => {},
                    Dash | DashDash | Slash => return ArgResult::Unrecognized,
                }
                self.arg_index += 1;

                return ArgResult::Ok(Check);
            }

            Compile | CompileAsm | CompileObj
            | Run | RunAsm | RunObj => {
                let (command_str, command_kay, command_asm, command_obj) = match command_flag {
                    Compile | CompileAsm | CompileObj => ("compile", Compile, CompileAsm, CompileObj),
                    Run | RunAsm | RunObj => ("run", Run, RunAsm, RunObj),

                    Help | HelpLong | HelpLongSlash |
                    HelpShort | HelpShortSlash |
                    HelpQuestion | HelpQuestionLong | HelpQuestionShort | HelpQuestionShortSlash |
                    Version | VersionLong | VersionLongSlash |
                    VersionShort | VersionShortSlash |
                    Check => unreachable!(),
                };

                if !arg.starts_with(command_str) {
                    return ArgResult::Unrecognized;
                }
                let separator_index = match prefix {
                    Empty => command_str.len(),
                    Dash | DashDash | Slash => return ArgResult::Unrecognized,
                };
                let command_flag_index = self.arg_index;

                let Some(separator) = arg.as_bytes().get(separator_index) else {
                    self.arg_index += 1;
                    return ArgResult::Ok(command_kay);
                };

                let start_of_language_index = match separator {
                    b'-' => separator_index + 1,
                    _ => return ArgResult::Unrecognized,
                };

                self.arg_index += 1;
                let language_str = &arg[start_of_language_index..];
                return match language_str {
                    "asm" => ArgResult::Ok(command_asm),
                    "obj" => ArgResult::Ok(command_obj),
                    "" => {
                        self.errors.push(Error {
                            kind: ErrorKind::MissingLanguage(command_flag),
                            arg_index: command_flag_index,
                        });
                        ArgResult::Err
                    }
                    _ => {
                        self.errors.push(Error {
                            kind: ErrorKind::UnrecognizedLanguageMode {
                                #[expect(clippy::cast_possible_truncation)]
                                start_of_language_index: start_of_language_index as u8,
                            },
                            arg_index: command_flag_index,
                        });
                        ArgResult::Err
                    },
                };
            }
        }
    }

    fn parse_out_path(
        &mut self,
        arg: &'args str,
        prefix: FlagPrefix,
    ) -> ArgResult<OutPath<'args>> {
        use FlagPrefix::{Dash, DashDash, Empty, Slash};
        const OUTPUT_LONG: &str = "output";
        const OUTPUT_SHORT: &str = "o";

        let (out_flag, separator_index, prefix_len) = if arg.starts_with(OUTPUT_LONG) {
            let (flag, prefix_len) = match prefix {
                DashDash => (OutputFlag::Long, 2),
                Slash => (OutputFlag::LongSlash, 1),
                Empty | Dash => return ArgResult::Unrecognized,
            };
            (flag, OUTPUT_LONG.len(), prefix_len)
        } else if arg.starts_with(OUTPUT_SHORT) {
            let (flag, prefix_len) = match prefix {
                Dash => (OutputFlag::Short, 1),
                Slash => (OutputFlag::ShortSlash, 1),
                Empty | DashDash => return ArgResult::Unrecognized,
            };
            (flag, OUTPUT_SHORT.len(), prefix_len)
        } else {
            return ArgResult::Unrecognized;
        };

        let (out_path_str, out_path_index, start_of_path_character_index) = 'out_path_str: {
            let out_path_arg_index = self.arg_index;

            let Some(separator) = arg.as_bytes().get(separator_index) else {
                self.arg_index += 1;
                let Some(out_path_str) = self.get_current_arg() else {
                    self.errors.push(Error {
                        kind: ErrorKind::MustBeFollowedOutputDirectoryPath(out_flag),
                        arg_index: out_path_arg_index,
                    });
                    return ArgResult::Err;
                };

                let out_path_index = self.arg_index;
                break 'out_path_str (out_path_str, out_path_index, 0);
            };

            let start_of_path_index = match separator {
                b'=' | b':' => separator_index + 1,
                _ => return ArgResult::Unrecognized,
            };
            #[expect(clippy::cast_possible_truncation)]
            (
                &arg[start_of_path_index..],
                out_path_arg_index,
                (start_of_path_index + prefix_len) as u8,
            )
        };
        self.arg_index += 1;

        let out_path = OutPath {
            path: Path::new(out_path_str),
            arg_index: out_path_index,
            start_of_path_character_index,
        };
        return ArgResult::Ok(out_path);
    }

    fn parse_src_path(
        &mut self,
        arg: &'args str,
    ) -> SrcPath<'args> {
        let src_path_index = self.arg_index;
        self.arg_index += 1;

        let src_path = SrcPath {
            path: Path::new(arg),
            arg_index: src_path_index,
        };
        return src_path;
    }

    fn set_command(&mut self, command_flag: CommandFlag, command_flag_index: usize) {
        use CommandFlag::{
            Help, HelpLong, HelpLongSlash,
            HelpShort, HelpShortSlash,
            HelpQuestion, HelpQuestionLong, HelpQuestionShort, HelpQuestionShortSlash,
            Version, VersionLong, VersionLongSlash,
            VersionShort, VersionShortSlash,
            Check,
            Compile, CompileAsm, CompileObj,
            Run, RunAsm, RunObj,
        };

        match command_flag {
            Help | HelpLong | HelpLongSlash
            | HelpShort | HelpShortSlash
            | HelpQuestion | HelpQuestionLong | HelpQuestionShort | HelpQuestionShortSlash => {
                // always setting the help command regardless of where it is found
                self.command_flag = Some((command_flag, command_flag_index));
            }

            Version | VersionLong | VersionLongSlash
            | VersionShort | VersionShortSlash => match &self.command_flag {
                Some((previous_command_flag, _)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::CommandAlreadySelected {
                            current: command_flag,
                            previous: *previous_command_flag,
                        },
                        arg_index: command_flag_index,
                    });
                },
                None => self.command_flag = Some((command_flag, command_flag_index)),
            }

            Check
            | Compile | CompileAsm | CompileObj
            | Run | RunAsm | RunObj => match &self.command_flag {
                Some((
                    Help | HelpLong | HelpLongSlash
                    | HelpShort | HelpShortSlash
                    | HelpQuestion | HelpQuestionLong | HelpQuestionShort | HelpQuestionShortSlash
                    | Version | VersionLong | VersionLongSlash
                    | VersionShort | VersionShortSlash
                , _)) => {
                    // make sure the command is properly formatted
                },
                Some((previous_command_flag @ (
                    Check
                    | Compile | CompileAsm | CompileObj
                    | Run | RunAsm | RunObj
                ), _)) => {
                    self.errors.push(Error {
                        kind: ErrorKind::CommandAlreadySelected {
                            current: command_flag,
                            previous: *previous_command_flag,
                        },
                        arg_index: command_flag_index,
                    });
                },
                None => self.command_flag = Some((command_flag, command_flag_index)),
            }
        }
    }

    fn get_command_and_report_errors(&mut self) -> Result<Command<'args>, ()> {
        use CommandFlag::{
            Help, HelpLong, HelpLongSlash,
            HelpShort, HelpShortSlash,
            HelpQuestion, HelpQuestionLong, HelpQuestionShort, HelpQuestionShortSlash,
            Version, VersionLong, VersionLongSlash,
            VersionShort, VersionShortSlash,
            Check,
            Compile, CompileAsm, CompileObj,
            Run, RunAsm, RunObj,
        };

        let verbosity = self.verbosity;

        let Some((command_flag, command_flag_index)) = self.command_flag else {
            let _src_path_option = self.get_src_path_and_report_errors();
            let _out_path_option = self.get_out_path_and_report_errors();
            return Ok(Command::Help);
        };

        if let
            Help | HelpLong | HelpLongSlash
            | HelpShort | HelpShortSlash
            | HelpQuestion | HelpQuestionLong | HelpQuestionShort | HelpQuestionShortSlash
        = command_flag {
            return Ok(Command::Help);
        }

        if let
            Version | VersionLong | VersionLongSlash
            | VersionShort | VersionShortSlash
        = command_flag {
            return Ok(Command::Version);
        }

        let src_path_option = self.get_src_path_and_report_errors();
        let Some(src_path) = src_path_option else {
            self.errors.push(Error {
                kind: ErrorKind::MissingSrcPath(command_flag),
                arg_index: command_flag_index,
            });
            return Err(());
        };
        let src_path_path = src_path.path;

        let out_path_option = self.get_out_path_and_report_errors();

        if let Check = command_flag {
            let Some(out_path) = out_path_option else {
                return Ok(Command::Check { src_path: src_path_path, verbosity });
            };
            let out_path_start_of_path_character_index = out_path.start_of_path_character_index;
            let out_path_arg_index = out_path.arg_index;
            self.errors.push(Error {
                kind: ErrorKind::CannotUseOutputDirectoryPathWithCheckCommand {
                    start_of_out_path_index: out_path_start_of_path_character_index,
                },
                arg_index: out_path_arg_index,
            });
            return Err(());
        }

        let Some(out_path) = out_path_option else {
            self.errors.push(Error {
                kind: ErrorKind::MissingOutputDirectoryPath(command_flag),
                arg_index: command_flag_index,
            });
            return Err(());
        };
        let out_path_path = out_path.path;

        return match command_flag {
            Compile => Ok(Command::Compile {
                src_path: src_path_path,
                verbosity,
                out_path: out_path_path,
                language: Language::Kay,
            }),
            CompileAsm => Ok(Command::Compile {
                src_path: src_path_path,
                verbosity,
                out_path: out_path_path,
                language: Language::Asm,
            }),
            CompileObj => Ok(Command::Compile {
                src_path: src_path_path,
                verbosity,
                out_path: out_path_path,
                language: Language::Obj,
            }),
            Run => Ok(Command::Run {
                src_path: src_path_path,
                verbosity,
                out_path: out_path_path,
                language: Language::Kay,
            }),
            RunAsm => Ok(Command::Run {
                src_path: src_path_path,
                verbosity,
                out_path: out_path_path,
                language: Language::Asm,
            }),
            RunObj => Ok(Command::Run {
                src_path: src_path_path,
                verbosity,
                out_path: out_path_path,
                language: Language::Obj,
            }),
            Help | HelpLong | HelpLongSlash
            | HelpShort | HelpShortSlash
            | HelpQuestion | HelpQuestionLong | HelpQuestionShort | HelpQuestionShortSlash
            | Version | VersionLong | VersionLongSlash
            | VersionShort | VersionShortSlash
            | Check => unreachable!(),
        };
    }

    fn get_src_path_and_report_errors(&mut self) -> Option<&SrcPath<'args>> {
        let mut src_paths = self.src_paths.iter();
        let src_path = src_paths.next()?;
        if !src_path.path.is_file() {
            self.errors.push(Error {
                kind: ErrorKind::MustBeAFilePath,
                arg_index: src_path.arg_index,
            });
        }

        for SrcPath { path, arg_index } in src_paths {
            if !path.is_file() {
                self.errors.push(Error {
                    kind: ErrorKind::MustBeAFilePath,
                    arg_index: *arg_index,
                });
            }
            self.errors.push(Error {
                kind: ErrorKind::SrcPathAlreadySelected {
                    previous_src_path_index: src_path.arg_index
                },
                arg_index: *arg_index,
            });
        }

        return Some(src_path);
    }

    fn get_out_path_and_report_errors(&mut self) -> Option<&OutPath<'args>> {
        let mut out_paths = self.out_paths.iter();
        let out_path = out_paths.next()?;
        if out_path.path.is_file() {
            self.errors.push(Error {
                kind: ErrorKind::MustBeADirectoryPath {
                    start_of_path_index: out_path.start_of_path_character_index,
                },
                arg_index: out_path.arg_index,
            });
        }

        for OutPath { path, arg_index, start_of_path_character_index } in out_paths {
            if path.is_file() {
                self.errors.push(Error {
                    kind: ErrorKind::MustBeADirectoryPath {
                        start_of_path_index: *start_of_path_character_index,
                    },
                    arg_index: *arg_index,
                });
            }
            self.errors.push(Error {
                kind: ErrorKind::OutputDirectoryPathAlreadySelected {
                    start_of_current_out_path_index: *start_of_path_character_index,
                    previous_out_path_index: out_path.arg_index,
                },
                arg_index: *arg_index,
            });
        }

        return Some(out_path);
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ErrorKind {
    Empty,

    MissingLanguage(CommandFlag),
    MustBeFollowedByColorMode(ColorFlag),
    MissingColorMode(ColorFlag),
    UnrecognizedColorMode { start_of_color_index: u8 },
    UnrecognizedLanguageMode { start_of_language_index: u8 },
    MustBeFollowedOutputDirectoryPath(OutputFlag),

    OutputDirectoryPathAlreadySelected {
        start_of_current_out_path_index: u8,
        previous_out_path_index: usize,
    },
    SrcPathAlreadySelected {
        previous_src_path_index: usize,
    },
    CommandAlreadySelected { current: CommandFlag, previous: CommandFlag },

    MustBeADirectoryPath { start_of_path_index: u8 },
    MissingOutputDirectoryPath(CommandFlag),
    MustBeAFilePath,
    MissingSrcPath(CommandFlag),
    // IDEA(stefano): point to the flag instead of the path
    StrayOutputDirectoryFlag { start_of_path_index: u8 },
    StraySrcPath,
    // IDEA(stefano): point to the flag instead of the path
    CannotUseOutputDirectoryPathWithCheckCommand { start_of_out_path_index: u8 },

    Unrecognized,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    // IDEA(stefano): use a u16/u32, who needs more than 65K/4G arguments?
    pub arg_index: usize,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Errors<'executable_name, 'args, S: AsRef<str>> {
    pub executable_name: Option<&'executable_name Path>,
    pub args: &'args [S],
    pub errors: Vec<Error>,
}

impl<S: AsRef<str>> Display for Errors<'_, '_, S> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut args_text = String::new();

        let mut pointers_offset = if let Some(executable_name) = self.executable_name {
            _ = write!(args_text, "{} ", executable_name.display());
            args_text.len()
        } else {
            0
        };

        let mut arg_index = 0;
        while arg_index < self.args.len() - 1 {
            let arg = &self.args[arg_index].as_ref();
            arg_index += 1;
            _ = write!(args_text, "{arg} ");
        }

        let last_arg = &self.args[arg_index].as_ref();
        _ = write!(args_text, "{last_arg}");

        let mut error_message = String::new();
        let mut error_cause_message = String::new();

        arg_index = 0;
        for Error { kind, arg_index: erroneous_arg_index } in &self.errors {
            // Note: these two loops are mutually exclusive and avoid extra checking
            while arg_index < *erroneous_arg_index {
                let arg = &self.args[arg_index].as_ref();
                arg_index += 1;
                pointers_offset += arg.len() + 1; // + 1 to account for the space between args
            }
            while arg_index > *erroneous_arg_index {
                arg_index -= 1;
                let arg = &self.args[arg_index].as_ref();
                pointers_offset -= arg.len() + 1; // + 1 to account for the space between args
            }

            let erroneous_arg = &self.args[arg_index].as_ref();
            let pointers_count = match erroneous_arg.len() {
                0 => 1, // empty arguments will at least get one pointer
                other => other,
            };

            let mut pointers_offset_inside_arg = 0;
            error_message.clear();
            error_cause_message.clear();
            match kind {
                ErrorKind::Empty => {
                    _ = write!(error_message, "invalid argument");
                    _ = write!(error_cause_message, "cannot be empty");
                },

                ErrorKind::MissingLanguage(command) => {
                    _ = write!(error_message, "invalid '{command}' command");
                    _ = write!(
                        error_cause_message,
                        "missing language '{asm}' or '{obj}'",
                        asm = Language::Asm,
                        obj = Language::Obj,
                    );
                },
                ErrorKind::MustBeFollowedByColorMode(flag) => {
                    _ = write!(error_message, "invalid '{flag}' option");
                    _ = write!(
                        error_cause_message,
                        "must be followed by '{auto}', '{always}' or '{never}'",
                        auto = Color::Auto,
                        always = Color::Always,
                        never = Color::Never,
                    );
                },
                ErrorKind::MissingColorMode(flag) => {
                    _ = write!(error_message, "invalid '{flag}' option");
                    _ = write!(
                        error_cause_message,
                        "missing color mode '{auto}', '{always}' or '{never}'",
                        auto = Color::Auto,
                        always = Color::Always,
                        never = Color::Never,
                    );
                },
                ErrorKind::UnrecognizedColorMode { start_of_color_index } => {
                    pointers_offset_inside_arg = *start_of_color_index as usize;
                    let color = &erroneous_arg[*start_of_color_index as usize..];

                    _ = write!(error_message, "unrecognized color mode '{color}'");
                    _ = write!(
                        error_cause_message,
                        "must be one of '{auto}', '{always}' or '{never}'",
                        auto = Color::Auto,
                        always = Color::Always,
                        never = Color::Never,
                    );
                },
                ErrorKind::UnrecognizedLanguageMode { start_of_language_index } => {
                    pointers_offset_inside_arg = *start_of_language_index as usize;
                    let language = &erroneous_arg[*start_of_language_index as usize..];

                    _ = write!(error_message, "unrecognized language mode '{language}'");
                    _ = write!(
                        error_cause_message,
                        "must be one of '{asm}' or '{obj}'",
                        asm = Language::Asm,
                        obj = Language::Obj,
                    );
                },
                ErrorKind::MustBeFollowedOutputDirectoryPath(option) => {
                    _ = write!(error_message, "invalid '{option}' option");
                    _ = write!(error_cause_message, "must be followed by an output directory path");
                },

                ErrorKind::OutputDirectoryPathAlreadySelected {
                    start_of_current_out_path_index,
                    previous_out_path_index,
                } => {
                    pointers_offset_inside_arg = *start_of_current_out_path_index as usize;
                    let current_out_path = &erroneous_arg[pointers_offset_inside_arg..];
                    let previous_out_path = self.args[*previous_out_path_index].as_ref();

                    _ = write!(error_message, "invalid '{current_out_path}' output directory argument");
                    _ = write!(
                        error_cause_message,
                        "already selected '{previous_out_path}' output directory path"
                    );
                },
                ErrorKind::SrcPathAlreadySelected { previous_src_path_index } => {
                    let current_src_path = *erroneous_arg;
                    let previous_src_path = self.args[*previous_src_path_index].as_ref();

                    _ = write!(error_message, "invalid '{current_src_path}' source file path argument");
                    _ = write!(
                        error_cause_message,
                        "already selected '{previous_src_path}' source file path"
                    );
                },
                ErrorKind::CommandAlreadySelected { current, previous } => {
                    _ = write!(error_message, "invalid '{current}' command");
                    _ = write!(
                        error_cause_message,
                        "cannot use '{current}' because '{previous}' was already selected"
                    );
                },

                ErrorKind::MustBeADirectoryPath { start_of_path_index } => {
                    pointers_offset_inside_arg = *start_of_path_index as usize;
                    let path = &erroneous_arg[pointers_offset_inside_arg..];

                    _ = write!(error_message, "invalid '{path}' path");
                    _ = write!(error_cause_message, "must be a directory path");
                },
                ErrorKind::MissingOutputDirectoryPath(command) => {
                    _ = write!(error_message, "invalid '{command}' command");
                    _ = write!(error_cause_message, "missing output directory path");
                },
                ErrorKind::MustBeAFilePath => {
                    _ = write!(error_message, "invalid '{erroneous_arg}' path");
                    _ = write!(error_cause_message, "must be a source file path");
                },
                ErrorKind::MissingSrcPath(command) => {
                    _ = write!(error_message, "invalid '{command}' command");
                    _ = write!(error_cause_message, "missing source file path");
                },
                ErrorKind::StrayOutputDirectoryFlag { start_of_path_index } => {
                    pointers_offset_inside_arg = *start_of_path_index as usize;
                    let current_out_path = &erroneous_arg[pointers_offset_inside_arg..];

                    _ = write!(error_message, "stray '{current_out_path}' output directory argument");
                    _ = write!(
                        error_cause_message,
                        "can only be used with a '{compile}' or '{run}' command",
                        compile = CommandFlag::Compile,
                        run = CommandFlag::Run,
                    );
                },
                ErrorKind::StraySrcPath => {
                    let current_src_path = *erroneous_arg;

                    _ = write!(error_message, "stray '{current_src_path}' source file argument");
                    _ = write!(
                        error_cause_message,
                        "can only be used with a '{check}', '{compile}' or '{run}' command",
                        check = CommandFlag::Check,
                        compile = CommandFlag::Compile,
                        run = CommandFlag::Run,
                    );
                },
                ErrorKind::CannotUseOutputDirectoryPathWithCheckCommand {
                    start_of_out_path_index,
                } => {
                    pointers_offset_inside_arg = *start_of_out_path_index as usize;
                    let current_out_path = &erroneous_arg[pointers_offset_inside_arg..];

                    _ = write!(error_message, "invalid '{current_out_path}' output directory argument");
                    _ = write!(
                        error_cause_message,
                        "cannot be used with a '{check}' command",
                        check = CommandFlag::Check
                    );
                },

                ErrorKind::Unrecognized => {
                    _ = write!(error_message, "unrecognized '{erroneous_arg}' argument");
                    _ = write!(error_cause_message, "unrecognized");
                },
            }

            let display_pointers_offset = pointers_offset + pointers_offset_inside_arg;
            let display_pointers_count = pointers_count - pointers_offset_inside_arg;
            let error = MsgWithCauseUnderText {
                kind: &ERROR,
                message: &error_message,
                cause: &error_cause_message,
                line_text: &args_text,
                #[expect(clippy::cast_possible_truncation)]
                pointers_offset: display_pointers_offset as offset32,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: display_pointers_count as offset32,
            };
            writeln!(f, "{error}\n")?;
        }
        return Ok(());
    }
}

#[expect(clippy::missing_trait_methods)]
impl<S: AsRef<str> + core::fmt::Debug> core::error::Error for Errors<'_, '_, S> {}
