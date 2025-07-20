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
        $($visibility static $step_name: Colored<&str> = Colored { text: $text, fg: $fg, bg: $bg, flags: $flags };)+
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
    Run                    = flag!(Self::RUN,           Empty,    Long),
}

#[rustfmt::skip]
impl CommandFlag {
    const HELP: u8          = 0b0000_0000;
    const HELP_QUESTION: u8 = 0b0000_0001;
    const VERSION: u8       = 0b0000_0010;
    const CHECK: u8         = 0b0000_0100;
    const COMPILE: u8       = 0b0000_0101;
    const RUN: u8           = 0b0000_0110;
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
            Self::Run                    => write!(f, "run"),
        };
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub enum Command<'args> {
    #[default]
    Help,
    Version,
    Check {
        src_path: &'args Path,
        verbosity: Verbosity,
    },

    // IDEA(stefano): add `compile-*` variations
    Compile {
        language: Language,
        src_path: &'args Path,
        out_path: &'args Path,
        verbosity: Verbosity,
    },
    // IDEA(stefano): add `run-*` variations
    Run {
        language: Language,
        src_path: &'args Path,
        out_path: &'args Path,
        verbosity: Verbosity,
    },
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum LanguageFlag {
    KayLong  = flag!(Language::Kay as u8, DashDash, Long),
    KaySlash = flag!(Language::Kay as u8, Slash,    Long),
    AsmLong  = flag!(Language::Asm as u8, DashDash, Long),
    AsmSlash = flag!(Language::Asm as u8, Slash,    Long),
    ObjLong  = flag!(Language::Obj as u8, Dash,     Long),
    ObjSlash = flag!(Language::Obj as u8, Slash,    Long),
}

impl Display for LanguageFlag {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::KayLong  => write!(f, "--kay"),
            Self::KaySlash => write!(f, "/kay"),
            Self::AsmLong  => write!(f, "--asm"),
            Self::AsmSlash => write!(f, "/asm"),
            Self::ObjLong  => write!(f, "--obj"),
            Self::ObjSlash => write!(f, "/obj"),
        };
    }
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
        static VERSION: Colored<&str> = Colored { text: env!("CARGO_PKG_VERSION"), fg, bg, flags };

        self.color.set(&std::io::stdout());
        return write!(f, "Kaylang compiler, version {VERSION}");
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Help {
    pub color: Color,
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
        const fg: Fg = Fg::White;
        const bg: Bg = Bg::Default;
        const flags: ansi_flag = AnsiFlag::Bold as ansi_flag;
        static USAGE:     Colored<&str> = Colored { text: "Usage",     fg, bg, flags };
        static OPTIONS:   Colored<&str> = Colored { text: "Options",   fg, bg, flags };
        static COMMAND:   Colored<&str> = Colored { text: "Command",   fg, bg, flags };
        static MODE:      Colored<&str> = Colored { text: "Mode",      fg, bg, flags };
        static LANGUAGE:  Colored<&str> = Colored { text: "Language",  fg, bg, flags };
        static FILE:      Colored<&str> = Colored { text: "file",      fg, bg, flags };
        static PATH:      Colored<&str> = Colored { text: "path",      fg, bg, flags };
        static OUTPUT:    Colored<&str> = Colored { text: "Output",    fg, bg, flags };
        static VERBOSITY: Colored<&str> = Colored { text: "Verbosity", fg, bg, flags };

        return write!(
            f,
            r"{Version}

{USAGE}: {executable_name} [{OPTIONS}] [{COMMAND}]

[{OPTIONS}]:
    {__color}, {Scolor}, {_c}, {Sc} <{MODE}>

    <{MODE}> (supports '*-{MODE}' and '*={MODE}' variations: '-c=auto'):
        {auto} (default)    only print colored output if supported
        {always}            always print colored output, even if not supported
        {never}             never print colored output

[{COMMAND}]s:
    {help},    {__help},    {Shelp},    {_h}, {Sh}, {hq}, {__hq}, {_hq}, {Shq}
        Display this message (default)

    {version}, {__version}, {Sversion}, {_v}, {Sv}
        Display the compiler version

    {check}              <{FILE}>          [{VERBOSITY}]
        Check the source code for correctness

    {compile} [{LANGUAGE}] <{FILE}> <{OUTPUT}> [{VERBOSITY}]
        Compile the source code down to an executable

    {run}     [{LANGUAGE}] <{FILE}> <{OUTPUT}> [{VERBOSITY}]
        Compile and run the generated executable

    [{LANGUAGE}]:
        {__kay}, {Skay} (default)   Compile <{FILE}> as a kay file
        {__asm}, {Sasm}             Compile <{FILE}> as an assembly file
        {__obj}, {Sobj}             Compile <{FILE}> as an object file

    <{OUTPUT}>:
        {__output}, {Soutput}, {_o}, {So} <{PATH}>

        <{PATH}> (supports '*={PATH}' variations: '-o=out'):
            Folder to populate with compilation artifacts

    [{VERBOSITY}]:
        {__quiet},   {Squiet},   {_q}, {Sq}
            Don't display any compilation information

        {__Verbose}, {SVerbose}, {_V}, {SV}
            Display extra compilation information",

            Version = Version { color: self.color },
            executable_name = self.executable_name.display(),

            __color = ColorFlag::Long,
            Scolor = ColorFlag::LongSlash,
            _c = ColorFlag::Short,
            Sc = ColorFlag::ShortSlash,
            auto = Color::Auto,
            always = Color::Always,
            never = Color::Never,

            help = CommandFlag::Help,
            __help = CommandFlag::HelpLong,
            Shelp = CommandFlag::HelpLongSlash,
            _h = CommandFlag::HelpShort,
            Sh = CommandFlag::HelpShortSlash,
            hq = CommandFlag::HelpQuestion,
            __hq = CommandFlag::HelpQuestionLong,
            _hq = CommandFlag::HelpQuestionShort,
            Shq = CommandFlag::HelpQuestionShortSlash,

            version = CommandFlag::Version,
            __version = CommandFlag::VersionLong,
            Sversion = CommandFlag::VersionLongSlash,
            _v = CommandFlag::VersionShort,
            Sv = CommandFlag::VersionShortSlash,

            check = CommandFlag::Check,
            compile = CommandFlag::Compile,
            run = CommandFlag::Run,

            __kay = LanguageFlag::KayLong,
            Skay = LanguageFlag::KaySlash,
            __asm = LanguageFlag::AsmLong,
            Sasm = LanguageFlag::AsmSlash,
            __obj = LanguageFlag::ObjLong,
            Sobj = LanguageFlag::ObjSlash,

            __output = OutputFlag::Long,
            Soutput = OutputFlag::LongSlash,
            _o = OutputFlag::Short,
            So = OutputFlag::ShortSlash,

            __quiet = VerbosityFlag::QuietLong,
            Squiet = VerbosityFlag::QuietLongSlash,
            _q = VerbosityFlag::QuietShort,
            Sq = VerbosityFlag::QuietShortSlash,
            __Verbose = VerbosityFlag::VerboseLong,
            SVerbose = VerbosityFlag::VerboseLongSlash,
            _V = VerbosityFlag::VerboseShort,
            SV = VerbosityFlag::VerboseShortSlash,
        );
    }
}

#[derive(Debug)]
enum ArgResult<P> {
    Ok(P),
    Err,
    Unrecognized,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Args<'args> {
    pub command: Result<Command<'args>, Vec<(ErrorKind, usize)>>,
    pub color: Color,
}

impl Default for Args<'_> {
    fn default() -> Self {
        return Self { color: Color::Auto, command: Ok(Command::Help) };
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArgsParser<'args> {
    args: &'args [String],
    arg_index: usize,

    color: Color,
    selected_command: Option<(CommandFlag, Command<'args>)>,
    errors: Vec<(ErrorKind, usize)>,
}

impl<'args> ArgsParser<'args> {
    // IDEA(stefano): make help and version commands collide with other commands
    // i.e.: `kay run file.txt help` should raise an error
    #[must_use]
    pub fn parse(args: &'args [String]) -> Args<'args> {
        use FlagPrefix::{Dash, DashDash, Empty, Slash};

        let mut parser = Self {
            args,
            arg_index: 0,

            color: Color::Auto,
            selected_command: None,
            errors: Vec::new(),
        };

        'next_arg: while let Some(raw_arg) = parser.current_arg() {
            let current_flag_index = parser.arg_index;
            if raw_arg.len() == 0 {
                parser.errors.push((ErrorKind::Empty, current_flag_index));
                parser.arg_index += 1;
                continue 'next_arg;
            }

            let (prefix, arg) = Self::split_prefix(raw_arg);

            'color: {
                let color = match parser.parse_color(prefix, arg) {
                    ArgResult::Ok((color, _)) => color,
                    ArgResult::Err => continue 'next_arg,
                    ArgResult::Unrecognized => break 'color,
                };

                parser.color = color;
                continue 'next_arg;
            }

            'help: {
                let flag = match arg {
                    "help" => match prefix {
                        Empty => CommandFlag::Help,
                        DashDash => CommandFlag::HelpLong,
                        Slash => CommandFlag::HelpLongSlash,
                        Dash => break 'help,
                    },
                    "h" => match prefix {
                        Dash => CommandFlag::HelpShort,
                        Slash => CommandFlag::HelpShortSlash,
                        Empty | DashDash => break 'help,
                    },
                    "?" => match prefix {
                        Empty => CommandFlag::HelpQuestion,
                        DashDash => CommandFlag::HelpQuestionLong,
                        Dash => CommandFlag::HelpQuestionShort,
                        Slash => CommandFlag::HelpQuestionShortSlash,
                    },
                    _ => break 'help,
                };
                parser.arg_index += 1;

                if let Some((previous_flag, Command::Help | Command::Version)) =
                    parser.selected_command
                {
                    parser.errors.push((
                        ErrorKind::CommandAlreadySelected {
                            current: flag,
                            previous: previous_flag,
                        },
                        current_flag_index,
                    ));
                    continue 'next_arg;
                }
                parser.selected_command = Some((flag, Command::Help));
                continue 'next_arg;
            }

            'version: {
                let flag = match arg {
                    "version" => match prefix {
                        Empty => CommandFlag::Version,
                        DashDash => CommandFlag::VersionLong,
                        Slash => CommandFlag::VersionLongSlash,
                        Dash => break 'version,
                    },
                    "v" => match prefix {
                        Dash => CommandFlag::VersionShort,
                        Slash => CommandFlag::VersionShortSlash,
                        Empty | DashDash => break 'version,
                    },
                    _ => break 'version,
                };
                parser.arg_index += 1;

                if let Some((previous_flag, Command::Help | Command::Version)) =
                    parser.selected_command
                {
                    parser.errors.push((
                        ErrorKind::CommandAlreadySelected {
                            current: flag,
                            previous: previous_flag,
                        },
                        current_flag_index,
                    ));
                    continue 'next_arg;
                }
                parser.selected_command = Some((flag, Command::Version));
                continue 'next_arg;
            }

            'check: {
                let command_flag = match arg {
                    "check" => match prefix {
                        Empty => CommandFlag::Check,
                        Dash | DashDash | Slash => break 'check,
                    },
                    _ => break 'check,
                };
                parser.arg_index += 1;

                let src_path = match parser.parse_src_path(command_flag, current_flag_index) {
                    ArgResult::Ok(src_path) => src_path,
                    ArgResult::Err | ArgResult::Unrecognized => continue 'next_arg,
                };

                let verbosity = parser.parse_verbosity_or_default();

                match &parser.selected_command {
                    None => {
                        let command = Command::Check { src_path, verbosity };
                        parser.selected_command = Some((command_flag, command));
                    },
                    Some((previous_command_flag, previous_command)) => match previous_command {
                        Command::Help | Command::Version => {
                            // make sure the command is properly formatted
                        },
                        Command::Check { .. } | Command::Compile { .. } | Command::Run { .. } => {
                            parser.errors.push((
                                ErrorKind::CommandAlreadySelected {
                                    current: command_flag,
                                    previous: *previous_command_flag,
                                },
                                current_flag_index,
                            ));
                        },
                    },
                }
                continue 'next_arg;
            }

            'compile: {
                let command_flag = match arg {
                    "compile" => match prefix {
                        Empty => CommandFlag::Compile,
                        Dash | DashDash | Slash => break 'compile,
                    },
                    _ => break 'compile,
                };
                parser.arg_index += 1;

                let language = parser.parse_language_or_default();

                let src_path_index = parser.arg_index;
                let src_path = match parser.parse_src_path(command_flag, current_flag_index) {
                    ArgResult::Ok(src_path) => src_path,
                    ArgResult::Err | ArgResult::Unrecognized => continue 'next_arg,
                };

                let out_path = match parser.parse_out_path_command(command_flag, src_path_index) {
                    ArgResult::Ok(out_path) => out_path,
                    ArgResult::Err | ArgResult::Unrecognized => continue 'next_arg,
                };

                let verbosity = parser.parse_verbosity_or_default();

                match &parser.selected_command {
                    None => {
                        let command = Command::Compile { language, src_path, out_path, verbosity };
                        parser.selected_command = Some((command_flag, command));
                    },
                    Some((previous_command_flag, previous_command)) => match previous_command {
                        Command::Help | Command::Version => {
                            // make sure the command is properly formatted
                        },
                        Command::Check { .. } | Command::Compile { .. } | Command::Run { .. } => {
                            parser.errors.push((
                                ErrorKind::CommandAlreadySelected {
                                    current: command_flag,
                                    previous: *previous_command_flag,
                                },
                                current_flag_index,
                            ));
                        },
                    },
                }
                continue 'next_arg;
            }

            'run: {
                let command_flag = match arg {
                    "run" => match prefix {
                        Empty => CommandFlag::Run,
                        Dash | DashDash | Slash => break 'run,
                    },
                    _ => break 'run,
                };
                parser.arg_index += 1;

                let language = parser.parse_language_or_default();

                let src_path_index = parser.arg_index;
                let src_path = match parser.parse_src_path(command_flag, current_flag_index) {
                    ArgResult::Ok(src_path) => src_path,
                    ArgResult::Err | ArgResult::Unrecognized => continue 'next_arg,
                };

                let out_path = match parser.parse_out_path_command(command_flag, src_path_index) {
                    ArgResult::Ok(out_path) => out_path,
                    ArgResult::Err | ArgResult::Unrecognized => continue 'next_arg,
                };

                let verbosity = parser.parse_verbosity_or_default();

                match &parser.selected_command {
                    None => {
                        let command = Command::Run { language, src_path, out_path, verbosity };
                        parser.selected_command = Some((command_flag, command));
                    },
                    Some((previous_command_flag, previous_command)) => match previous_command {
                        Command::Help | Command::Version => {
                            // make sure the command is properly formatted
                        },
                        Command::Check { .. } | Command::Compile { .. } | Command::Run { .. } => {
                            parser.errors.push((
                                ErrorKind::CommandAlreadySelected {
                                    current: command_flag,
                                    previous: *previous_command_flag,
                                },
                                current_flag_index,
                            ));
                        },
                    },
                }
                continue 'next_arg;
            }

            'stray_language: {
                let Some((_, flag)) = parser.parse_language(prefix, arg) else {
                    break 'stray_language;
                };
                parser.errors.push((ErrorKind::StrayLanguageOption(flag), current_flag_index));
                continue 'next_arg;
            }

            'stray_out_path: {
                let flag = match parser.parse_out_path(prefix, arg) {
                    ArgResult::Ok((_, flag)) => flag,
                    ArgResult::Err => continue 'next_arg,
                    ArgResult::Unrecognized => break 'stray_out_path,
                };

                parser
                    .errors
                    .push((ErrorKind::StrayOutputDirectoryOption(flag), current_flag_index));
                continue 'next_arg;
            }

            'stray_verbosity: {
                let Some((_, flag)) = parser.parse_verbosity(prefix, arg) else {
                    break 'stray_verbosity;
                };
                parser.errors.push((ErrorKind::StrayVerbosityOption(flag), current_flag_index));
                continue 'next_arg;
            }

            parser.errors.push((ErrorKind::Unrecognized, current_flag_index));
            parser.arg_index += 1;
        }

        if !parser.errors.is_empty() {
            return Args { color: parser.color, command: Err(parser.errors) };
        }

        let command = match parser.selected_command {
            Some((_, command)) => command,
            None => Command::Help,
        };

        return Args { color: parser.color, command: Ok(command) };
    }
}

impl<'args> ArgsParser<'args> {
    #[must_use]
    fn current_arg(&self) -> Option<&'args str> {
        if self.arg_index >= self.args.len() {
            return None;
        }

        let arg = &self.args[self.arg_index];
        return Some(arg.as_str());
    }

    #[must_use]
    fn split_prefix(arg: &'args str) -> (FlagPrefix, &'args str) {
        let arg_characters = arg.as_bytes();
        let split_prefix = match arg_characters.get(0) {
            Some(b'/') => {
                let content = &arg[1..];
                (FlagPrefix::Slash, content)
            },
            Some(b'-') => {
                if let Some(b'-') = arg_characters.get(1) {
                    let content = &arg[2..];
                    (FlagPrefix::DashDash, content)
                } else {
                    let content = &arg[1..];
                    (FlagPrefix::Dash, content)
                }
            },
            Some(_) | None => (FlagPrefix::Empty, arg),
        };

        return split_prefix;
    }
}

impl<'args> ArgsParser<'args> {
    #[must_use]
    fn parse_src_path(
        &mut self,
        command_flag: CommandFlag,
        command_flag_index: usize,
    ) -> ArgResult<&'args Path> {
        let Some(src_path_str) = self.current_arg() else {
            self.errors.push((
                ErrorKind::MustBeFollowedBySourceFilePath(command_flag),
                command_flag_index,
            ));
            return ArgResult::Err;
        };
        self.arg_index += 1;

        let src_path = Path::new(src_path_str);
        if !src_path.is_file() {
            self.errors.push((ErrorKind::MustBeAFilePath, self.arg_index));
            return ArgResult::Err;
        }
        return ArgResult::Ok(src_path);
    }

    #[must_use]
    fn parse_out_path(
        &mut self,
        prefix: FlagPrefix,
        arg: &'args str,
    ) -> ArgResult<(&'args Path, OutputFlag)> {
        use FlagPrefix::{Dash, DashDash, Empty, Slash};
        const OUTPUT_LONG: &str = "output";
        const OUTPUT_SHORT: &str = "o";

        let (flag, separator_index) = if arg.starts_with(OUTPUT_LONG) {
            let flag = match prefix {
                DashDash => OutputFlag::Long,
                Slash => OutputFlag::LongSlash,
                Empty | Dash => return ArgResult::Unrecognized,
            };
            (flag, OUTPUT_LONG.len())
        } else if arg.starts_with(OUTPUT_SHORT) {
            let flag = match prefix {
                Dash => OutputFlag::Short,
                Slash => OutputFlag::ShortSlash,
                Empty | DashDash => return ArgResult::Unrecognized,
            };
            (flag, OUTPUT_SHORT.len())
        } else {
            return ArgResult::Unrecognized;
        };

        let current_flag_index = self.arg_index;
        let (out_path_str, start_of_path_index) = 'out_path_str: {
            let Some(separator) = arg.as_bytes().get(separator_index) else {
                self.arg_index += 1;
                let Some(out_path_str) = self.current_arg() else {
                    self.errors
                        .push((ErrorKind::MissingOutputDirectoryPath(flag), current_flag_index));
                    return ArgResult::Err;
                };
                break 'out_path_str (out_path_str, None);
            };

            let start_of_path_index = match separator {
                b'=' => separator_index + 1,
                _ => return ArgResult::Unrecognized,
            };
            #[expect(clippy::cast_possible_truncation)]
            (&arg[start_of_path_index..], Some(start_of_path_index as u8))
        };
        self.arg_index += 1;

        let out_path = Path::new(out_path_str);
        if out_path.is_file() {
            let (flag_index, path_arg_index) = match start_of_path_index {
                Some(path_arg_index) => (current_flag_index, path_arg_index),
                None => (self.arg_index, 0),
            };
            self.errors.push((
                ErrorKind::MustBeADirectoryPath { start_of_path_index: path_arg_index },
                flag_index,
            ));
            return ArgResult::Err;
        }

        return ArgResult::Ok((out_path, flag));
    }

    #[must_use]
    fn parse_out_path_command(
        &mut self,
        command_flag: CommandFlag,
        src_path_index: usize,
    ) -> ArgResult<&'args Path> {
        let Some(out_path_flag_str) = self.current_arg() else {
            self.errors.push((ErrorKind::MustBeFollowedByOutputFlag(command_flag), src_path_index));
            return ArgResult::Err;
        };

        let (out_path_prefix, out_path_flag) = Self::split_prefix(out_path_flag_str);
        let out_path = match self.parse_out_path(out_path_prefix, out_path_flag) {
            ArgResult::Ok((out_path, _)) => out_path,
            ArgResult::Err => return ArgResult::Err,
            ArgResult::Unrecognized => {
                self.errors
                    .push((ErrorKind::MustBeFollowedByOutputFlag(command_flag), src_path_index));
                return ArgResult::Err;
            },
        };
        return ArgResult::Ok(out_path);
    }

    #[must_use]
    fn parse_color(
        &mut self,
        prefix: FlagPrefix,
        arg: &'args str,
    ) -> ArgResult<(Color, ColorFlag)> {
        use FlagPrefix::{Dash, DashDash, Empty, Slash};
        const COLOR_LONG: &str = "color";
        const COLOR_SHORT: &str = "c";

        let (flag, separator_index) = if arg.starts_with(COLOR_LONG) {
            let flag = match prefix {
                DashDash => ColorFlag::Long,
                Slash => ColorFlag::LongSlash,
                Empty | Dash => return ArgResult::Unrecognized,
            };
            (flag, COLOR_LONG.len())
        } else if arg.starts_with(COLOR_SHORT) {
            let flag = match prefix {
                Dash => ColorFlag::Short,
                Slash => ColorFlag::ShortSlash,
                Empty | DashDash => return ArgResult::Unrecognized,
            };
            (flag, COLOR_SHORT.len())
        } else {
            return ArgResult::Unrecognized;
        };

        let current_flag_index = self.arg_index;
        let (color_str, start_of_color_index) = 'color_str: {
            let Some(separator) = arg.as_bytes().get(separator_index) else {
                self.arg_index += 1;
                let Some(color_str) = self.current_arg() else {
                    self.errors
                        .push((ErrorKind::MustBeFollowedByColorMode(flag), current_flag_index));
                    return ArgResult::Err;
                };
                break 'color_str (color_str, None);
            };

            let start_of_color_index = match separator {
                b'-' | b'=' => separator_index + 1,
                _ => return ArgResult::Unrecognized,
            };

            #[expect(clippy::cast_possible_truncation)]
            (&arg[start_of_color_index..], Some(start_of_color_index as u8))
        };
        self.arg_index += 1;

        let color = match color_str {
            "auto" => Color::Auto,
            "always" => Color::Always,
            "never" => Color::Never,
            _ => {
                let (flag_index, color_arg_index) = match start_of_color_index {
                    Some(color_arg_index) => (current_flag_index, color_arg_index),
                    None => (self.arg_index, 0),
                };
                self.errors.push((
                    ErrorKind::UnrecognizedColorMode { start_of_color_index: color_arg_index },
                    flag_index,
                ));
                return ArgResult::Err;
            },
        };

        return ArgResult::Ok((color, flag));
    }

    #[must_use]
    fn parse_verbosity(
        &mut self,
        prefix: FlagPrefix,
        arg: &'args str,
    ) -> Option<(Verbosity, VerbosityFlag)> {
        use FlagPrefix::{Dash, DashDash, Empty, Slash};
        let verbosity_and_flag = match arg {
            "quiet" => match prefix {
                DashDash => (Verbosity::Quiet, VerbosityFlag::QuietLong),
                Slash => (Verbosity::Quiet, VerbosityFlag::QuietLongSlash),
                Empty | Dash => return None,
            },
            "q" => match prefix {
                Dash => (Verbosity::Quiet, VerbosityFlag::QuietShort),
                Slash => (Verbosity::Quiet, VerbosityFlag::QuietShortSlash),
                Empty | DashDash => return None,
            },
            "Verbose" => match prefix {
                DashDash => (Verbosity::Verbose, VerbosityFlag::VerboseLong),
                Slash => (Verbosity::Verbose, VerbosityFlag::VerboseLongSlash),
                Empty | Dash => return None,
            },
            "V" => match prefix {
                Dash => (Verbosity::Verbose, VerbosityFlag::VerboseShort),
                Slash => (Verbosity::Verbose, VerbosityFlag::VerboseShortSlash),
                Empty | DashDash => return None,
            },
            _ => return None,
        };
        self.arg_index += 1;

        return Some(verbosity_and_flag);
    }

    #[must_use]
    fn parse_verbosity_or_default(&mut self) -> Verbosity {
        let Some(verbosity_flag_str) = self.current_arg() else {
            return Verbosity::default();
        };

        let (verbosity_prefix, verbosity_flag) = Self::split_prefix(verbosity_flag_str);
        return match self.parse_verbosity(verbosity_prefix, verbosity_flag) {
            Some((verbosity, _)) => verbosity,
            None => Verbosity::default(),
        };
    }

    #[must_use]
    fn parse_language(
        &mut self,
        prefix: FlagPrefix,
        arg: &'args str,
    ) -> Option<(Language, LanguageFlag)> {
        use FlagPrefix::{Dash, DashDash, Empty, Slash};
        let language_and_flag = match arg {
            "kay" => match prefix {
                DashDash => (Language::Kay, LanguageFlag::KayLong),
                Slash => (Language::Kay, LanguageFlag::KaySlash),
                Empty | Dash => return None,
            },
            "asm" => match prefix {
                Dash => (Language::Asm, LanguageFlag::AsmLong),
                Slash => (Language::Asm, LanguageFlag::AsmSlash),
                Empty | DashDash => return None,
            },
            "obj" => match prefix {
                Dash => (Language::Obj, LanguageFlag::ObjLong),
                Slash => (Language::Obj, LanguageFlag::ObjSlash),
                Empty | DashDash => return None,
            },
            _ => return None,
        };
        self.arg_index += 1;

        return Some(language_and_flag);
    }

    #[must_use]
    fn parse_language_or_default(&mut self) -> Language {
        let Some(language_flag_str) = self.current_arg() else {
            return Language::default();
        };

        let (language_prefix, language_flag) = Self::split_prefix(language_flag_str);
        return match self.parse_language(language_prefix, language_flag) {
            Some((language, _)) => language,
            None => Language::default(),
        };
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ErrorKind {
    Empty,

    MustBeFollowedByColorMode(ColorFlag),
    UnrecognizedColorMode { start_of_color_index: u8 },

    MustBeFollowedBySourceFilePath(CommandFlag),
    MustBeAFilePath,

    MustBeFollowedByOutputFlag(CommandFlag),
    MissingOutputDirectoryPath(OutputFlag),
    MustBeADirectoryPath { start_of_path_index: u8 },

    StrayLanguageOption(LanguageFlag),
    StrayOutputDirectoryOption(OutputFlag),
    StrayVerbosityOption(VerbosityFlag),

    CommandAlreadySelected { current: CommandFlag, previous: CommandFlag },

    Unrecognized,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Errors<'executable_name, 'args> {
    pub executable_name: Option<&'executable_name Path>,
    pub args: &'args [String],
    pub errors: Vec<(ErrorKind, usize)>,
}

impl Display for Errors<'_, '_> {
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
            let arg = &self.args[arg_index];
            arg_index += 1;
            _ = write!(args_text, "{arg} ");
        }

        let last_arg = &self.args[arg_index];
        _ = write!(args_text, "{last_arg}");

        let mut error_message = String::new();
        let mut error_cause_message = String::new();

        arg_index = 0;
        for (kind, erroneous_arg_index) in &self.errors {
            // Note: these two loops are mutually exclusive and avoid extra checking
            while arg_index < *erroneous_arg_index {
                let arg = &self.args[arg_index];
                arg_index += 1;
                pointers_offset += arg.len() + 1; // + 1 to account for the space between args
            }
            while arg_index > *erroneous_arg_index {
                arg_index -= 1;
                let arg = &self.args[arg_index];
                pointers_offset -= arg.len() + 1; // + 1 to account for the space between args
            }

            let erroneous_arg = &self.args[arg_index];
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
                ErrorKind::UnrecognizedColorMode { start_of_color_index } => {
                    pointers_offset_inside_arg = *start_of_color_index as usize + 1;
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
                ErrorKind::CommandAlreadySelected { current, previous } => {
                    _ = write!(error_message, "invalid '{current}' command");
                    _ = write!(
                        error_cause_message,
                        "cannot use '{current}' because '{previous}' was already selected"
                    );
                },
                ErrorKind::MustBeFollowedBySourceFilePath(command) => {
                    _ = write!(error_message, "invalid '{command}' command");
                    _ = write!(error_cause_message, "must be followed by a source file path");
                },
                ErrorKind::MustBeAFilePath => {
                    _ = write!(error_message, "invalid '{erroneous_arg}' path");
                    _ = write!(error_cause_message, "must be a source file path");
                },
                ErrorKind::MustBeFollowedByOutputFlag(command) => {
                    _ = write!(error_message, "invalid '{command}' command");
                    _ = write!(
                        error_cause_message,
                        "must be followed by '{_o}' or '{__output}'",
                        _o = OutputFlag::Short,
                        __output = OutputFlag::Long,
                    );
                },
                ErrorKind::MissingOutputDirectoryPath(option) => {
                    _ = write!(error_message, "invalid '{option}' option");
                    _ = write!(error_cause_message, "must be followed by an output directory path");
                },
                ErrorKind::MustBeADirectoryPath { start_of_path_index } => {
                    pointers_offset_inside_arg = *start_of_path_index as usize + 1;
                    let path = &erroneous_arg[*start_of_path_index as usize..];

                    _ = write!(error_message, "invalid '{path}' path");
                    _ = write!(error_cause_message, "must be a directory path");
                },
                ErrorKind::StrayOutputDirectoryOption(option) => {
                    _ = write!(error_message, "stray '{option}' option");
                    _ = write!(
                        error_cause_message,
                        "can only be used after a '{compile}' or '{run}' command",
                        compile = CommandFlag::Compile,
                        run = CommandFlag::Run,
                    );
                },
                ErrorKind::StrayLanguageOption(option) => {
                    _ = write!(error_message, "stray '{option}' option");
                    _ = write!(
                        error_cause_message,
                        "can only be used after a '{check}', '{compile}' or '{run}' command",
                        check = CommandFlag::Check,
                        compile = CommandFlag::Compile,
                        run = CommandFlag::Run,
                    );
                },
                ErrorKind::StrayVerbosityOption(option) => {
                    _ = write!(error_message, "stray '{option}' option");
                    _ = write!(
                        error_cause_message,
                        "can only be used after a '{check}', '{compile}' or '{run}' command",
                        check = CommandFlag::Check,
                        compile = CommandFlag::Compile,
                        run = CommandFlag::Run,
                    );
                },
                ErrorKind::Unrecognized => {
                    _ = write!(error_message, "unrecognized '{erroneous_arg}' arg");
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
impl core::error::Error for Errors<'_, '_> {}
