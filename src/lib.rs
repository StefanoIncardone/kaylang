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

// main compilation steps (displayed when verbosity level is normal or verbose)
const STEP_FG: Fg = Fg::LightGreen;
const STEP_BG: Bg = Bg::Default;
const STEP_FLAGS: ansi_flag = AnsiFlag::Bold as ansi_flag;
const STEP_INDENT: usize = 0;
// TODO(stefano): change to const when upgrading rust version
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
// TODO(stefano): change to const when upgrading rust version
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

#[rustfmt::skip] pub static COULD_NOT_WRITE_COMPILED_CODE: Colored<&str> = Colored { text: "Could not write compile code", fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static COULD_NOT_RUN_ASSEMBLER:       Colored<&str> = Colored { text: "Could not run assembler",      fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static COULD_NOT_RUN_LINKER:          Colored<&str> = Colored { text: "Could not run linker",         fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static COULD_NOT_RUN_EXECUTABLE:      Colored<&str> = Colored { text: "Could not run executable",     fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static ASSEMBLING_ERROR:              Colored<&str> = Colored { text: "Assembling Error",             fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };
#[rustfmt::skip] pub static LINKING_ERROR:                 Colored<&str> = Colored { text: "Linking Error",                fg: ERR_FG, bg: ERR_BG, flags: ERR_FLAGS };

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
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum CommandFlag {
    Help                   = flag!(0b0000_0000, Empty,    Long),
    HelpLong               = flag!(0b0000_0000, DashDash, Long),
    HelpLongSlash          = flag!(0b0000_0000, Slash,    Long),
    HelpShort              = flag!(0b0000_0000, Dash,     Short),
    HelpShortSlash         = flag!(0b0000_0000, Slash,    Short),

    HelpQuestion           = flag!(0b0000_0001, Empty,    Long),
    HelpQuestionLong       = flag!(0b0000_0001, DashDash, Long),
    HelpQuestionShort      = flag!(0b0000_0001, Dash,     Short),
    HelpQuestionShortSlash = flag!(0b0000_0001, Slash,    Short),

    Version                = flag!(0b0000_0010, Empty,    Long),
    VersionLong            = flag!(0b0000_0010, DashDash, Long),
    VersionLongSlash       = flag!(0b0000_0010, Slash,    Long),
    VersionShort           = flag!(0b0000_0010, Dash,     Short),
    VersionShortSlash      = flag!(0b0000_0010, Slash,    Short),

    Check                  = flag!(0b0000_0100, Empty,    Long),
    Compile                = flag!(0b0000_0101, Empty,    Long),
    Run                    = flag!(0b0000_0110, Empty,    Long),
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

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum LanguageFlag {
    Kay      = flag!(0b0000_0000, DashDash, Long),
    KaySlash = flag!(0b0000_0000, Slash,    Long),
    Asm      = flag!(0b0000_0001, DashDash, Long),
    AsmSlash = flag!(0b0000_0001, Slash,    Long),
    Obj      = flag!(0b0000_0010, Dash,     Long),
    ObjSlash = flag!(0b0000_0010, Slash,    Long),
}

impl Display for LanguageFlag {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[rustfmt::skip]
        return match self {
            Self::Kay       => write!(f, "--kay"),
            Self::KaySlash  => write!(f, "/kay"),
            Self::Asm       => write!(f, "--asm"),
            Self::AsmSlash  => write!(f, "/asm"),
            Self::Obj       => write!(f, "--obj"),
            Self::ObjSlash  => write!(f, "/obj"),
        };
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
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

// IDEA(stefano): make mandatory
#[rustfmt::skip]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum VerbosityFlag {
    QuietLong         = flag!(0b0000_0000, DashDash, Long),
    QuietLongSlash    = flag!(0b0000_0000, Slash,    Long),
    QuietShort        = flag!(0b0000_0000, Dash,     Short),
    QuietShortSlash   = flag!(0b0000_0000, Slash,    Short),

    VerboseLong       = flag!(0b0000_0001, DashDash, Long),
    VerboseLongSlash  = flag!(0b0000_0001, Slash,    Long),
    VerboseShort      = flag!(0b0000_0001, Dash,     Short),
    VerboseShortSlash = flag!(0b0000_0001, Slash,    Short),
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
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(u8)]
pub enum Verbosity {
    #[default]
    Normal  = 0b0000_0000,
    Quiet   = 0b0000_0001,
    Verbose = 0b0000_0010,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    Help { executable_name: PathBuf },
    Version,
    Check { src_path: PathBuf, verbosity: Verbosity },
    Compile { language: Language, src_path: PathBuf, out_path: Option<PathBuf>, verbosity: Verbosity },
    Run { language: Language, src_path: PathBuf, out_path: Option<PathBuf>, verbosity: Verbosity },
}

impl Default for Command {
    #[inline(always)]
    fn default() -> Self {
        return Self::Help { executable_name: PathBuf::from("kay") };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Help {
    pub color: Color,
    pub executable_name: PathBuf,
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
        static MODE:      Colored<&str> = Colored { text: "mode",      fg, bg, flags };
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

    <{MODE}> (supports '* {MODE}', '*-{MODE}' and '*={MODE}' variations, eg: '-c=auto'):
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

    {compile} [{LANGUAGE}] <{FILE}> [{OUTPUT}] [{VERBOSITY}]
        Compile the source code down to an executable

    {run}     [{LANGUAGE}] <{FILE}> [{OUTPUT}] [{VERBOSITY}]
        Compile and run the generated executable

    [{LANGUAGE}]:
        {__kay}, {Skay} (default)   Compile <{FILE}> as a kay file
        {__asm}, {Sasm}             Compile <{FILE}> as an assembly file
        {__obj}, {Sobj}             Compile <{FILE}> as an object file

    [{OUTPUT}]:
        {__output}, {Soutput}, {_o}, {So} <{PATH}>

        <{PATH}>: Folder to populate with compilation artifacts (default: '.')

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

            __kay = LanguageFlag::Kay,
            Skay = LanguageFlag::KaySlash,
            __asm = LanguageFlag::Asm,
            Sasm = LanguageFlag::AsmSlash,
            __obj = LanguageFlag::Obj,
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
        let mut color = Color::Auto;
        color.set(&std::io::stderr());

        // IDEA(stefano): go back to good ol' index iterators
        let mut args_iter = args.iter().enumerate().peekable();
        let Some((_executable_name_index, executable_name)) = args_iter.next() else {
            return Err(Error::EmptyArgs);
        };

        let mut command_option: Option<(CommandFlag, Command)> = None;
        let mut errors = Vec::<(ErrorKind, usize)>::new();
        'args: while let Some((selected_flag_index, selected_flag)) = args_iter.next() {
            match selected_flag.as_str() {
                help_command @ (
                    "help"
                    | "--help"
                    | "/help"
                    | "-h"
                    | "/h"

                    | "?"
                    | "--?"
                    | "-?"
                    | "/?"
                ) => {
                    let help_flag = match help_command {
                        "help" => CommandFlag::Help,
                        "--help" => CommandFlag::HelpLong,
                        "/help" => CommandFlag::HelpLongSlash,
                        "-h" => CommandFlag::HelpShort,
                        "/h" => CommandFlag::HelpShortSlash,

                        "?" => CommandFlag::HelpQuestion,
                        "--?" => CommandFlag::HelpQuestionLong,
                        "-?" => CommandFlag::HelpQuestionShort,
                        "/?" => CommandFlag::HelpQuestionShortSlash,
                        _ => unreachable!(),
                    };

                    if let Some((previous_help_flag, Command::Help { .. } | Command::Version)) =
                        command_option
                    {
                        errors.push((
                            ErrorKind::CommandAlreadySelected {
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
                version_command @ (
                    "version"
                    | "--version"
                    | "/version"
                    | "-v"
                    | "/v"
                ) => {
                    let version_flag = match version_command {
                        "version" => CommandFlag::Version,
                        "--version" => CommandFlag::VersionLong,
                        "/version" => CommandFlag::VersionLongSlash,
                        "-v" => CommandFlag::VersionShort,
                        "/v" => CommandFlag::VersionShortSlash,
                        _ => unreachable!(),
                    };

                    if let Some((previous_version_flag, Command::Help { .. } | Command::Version)) =
                        command_option
                    {
                        errors.push((
                            ErrorKind::CommandAlreadySelected {
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
                        break 'args;
                    };

                    let src_path = Path::new(src_path_string);
                    if !src_path.is_file() {
                        errors.push((ErrorKind::MustBeAFilePath, src_path_index));
                    }

                    let verbosity =
                        if let Some((_verbosity_flag_index, verbosity_flag)) = args_iter.peek() {
                            match verbosity_flag.as_str() {
                                "--quiet" | "/quiet" | "-q" | "/q"  => {
                                    _ = args_iter.next();
                                    Verbosity::Quiet
                                }
                                "--Verbose" | "/Verbose" | "-V" | "/V" => {
                                    _ = args_iter.next();
                                    Verbosity::Verbose
                                }
                                _ => Verbosity::Normal,
                            }
                        } else {
                            Verbosity::Normal
                        };

                    match &command_option {
                        None => {
                            let mode =
                                Command::Check { src_path: src_path.to_owned(), verbosity };
                            command_option = Some((command_flag, mode));
                        }
                        Some((previous_command_flag, previous_command)) => match previous_command {
                            Command::Help { .. } | Command::Version => {
                                // this is just to make sure that commands are properly formatted,
                                // so we do nothing in the case where help or version commands were
                                // already selected
                            }
                            Command::Check { .. }
                            | Command::Compile { .. }
                            | Command::Run { .. } => {
                                errors.push((
                                    ErrorKind::CommandAlreadySelected {
                                        current: command_flag,
                                        previous: *previous_command_flag,
                                    },
                                    selected_flag_index,
                                ));
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

                    let language = if let Some((_, language_flag)) = args_iter.peek() {
                        match language_flag.as_str() {
                            "--kay" | "/kay" => {
                                _ = args_iter.next();
                                Language::Kay
                            }
                            "--asm" | "/asm" => {
                                _ = args_iter.next();
                                Language::Asm
                            }
                            "--obj" | "/obj" => {
                                _ = args_iter.next();
                                Language::Obj
                            }
                            _ => Language::Kay,
                        }
                    } else {
                        Language::Kay
                    };

                    let Some((src_path_index, src_path_string)) = args_iter.next() else {
                        errors.push((
                            ErrorKind::MustBeFollowedByASourceFilePath(command_flag),
                            selected_flag_index,
                        ));
                        break 'args;
                    };

                    let src_path = Path::new(src_path_string);
                    if !src_path.is_file() {
                        errors.push((ErrorKind::MustBeAFilePath, src_path_index));
                    }

                    let out_path = 'out_path: {
                        if let Some((peeked_out_flag_index, out_flag)) = args_iter.peek() {
                            let out_flag_index = *peeked_out_flag_index;
                            let out_option = match out_flag.as_str() {
                                "--output" | "/output" => {
                                    _ = args_iter.next();
                                    OutputFlag::Long
                                }
                                "-o" | "/o" => {
                                    _ = args_iter.next();
                                    OutputFlag::Short
                                }
                                _ => break 'out_path None,
                            };

                            let Some((out_path_index, out_path_string)) = args_iter.next() else {
                                errors.push((
                                    ErrorKind::MustBeFollowedByDirectoryPath(out_option),
                                    out_flag_index,
                                ));
                                break 'args;
                            };

                            let out_path = Path::new(out_path_string);
                            if out_path.is_file() {
                                errors.push((ErrorKind::MustBeADirectoryPath, out_path_index));
                            }

                            Some(out_path.to_owned())
                        } else {
                            None
                        }
                    };

                    let verbosity =
                        if let Some((_verbosity_flag_index, verbosity_flag)) = args_iter.peek() {
                            match verbosity_flag.as_str() {
                                "--quiet" | "/quiet" | "-q" | "/q" => {
                                    _ = args_iter.next();
                                    Verbosity::Quiet
                                }
                                "--Verbose" | "/Verbose" | "-V" | "/V" => {
                                    _ = args_iter.next();
                                    Verbosity::Verbose
                                }
                                _ => Verbosity::Normal,
                            }
                        } else {
                            Verbosity::Normal
                        };

                    match &command_option {
                        None => {
                            let mode = match command_flag {
                                CommandFlag::Compile => Command::Compile {
                                    language,
                                    src_path: src_path.to_owned(),
                                    out_path,
                                    verbosity,
                                },
                                CommandFlag::Run => Command::Run {
                                    language,
                                    src_path: src_path.to_owned(),
                                    out_path,
                                    verbosity,
                                },
                                CommandFlag::Help
                                | CommandFlag::HelpLong
                                | CommandFlag::HelpLongSlash
                                | CommandFlag::HelpShort
                                | CommandFlag::HelpShortSlash
                                | CommandFlag::HelpQuestion
                                | CommandFlag::HelpQuestionLong
                                | CommandFlag::HelpQuestionShort
                                | CommandFlag::HelpQuestionShortSlash
                                | CommandFlag::Version
                                | CommandFlag::VersionLong
                                | CommandFlag::VersionLongSlash
                                | CommandFlag::VersionShort
                                | CommandFlag::VersionShortSlash
                                | CommandFlag::Check => unreachable!(),
                            };

                            command_option = Some((command_flag, mode));
                        }
                        Some((previous_command_flag, previous_command)) => match previous_command {
                            Command::Help { .. } | Command::Version => {
                                // this is just to make sure that commands are properly formatted,
                                // so we do nothing in the case where help or version commands were
                                // already selected
                            }
                            Command::Check { .. }
                            | Command::Compile { .. }
                            | Command::Run { .. } => {
                                errors.push((
                                    ErrorKind::CommandAlreadySelected {
                                        current: command_flag,
                                        previous: *previous_command_flag,
                                    },
                                    selected_flag_index,
                                ));
                            }
                        },
                    }
                }
                out_flag @ ("--output" | "/output" | "-o" | "/o") => {
                    let flag = match out_flag {
                        "--output" => OutputFlag::Long,
                        "/output" => OutputFlag::LongSlash,
                        "-o" => OutputFlag::Short,
                        "/o" => OutputFlag::ShortSlash,
                        _ => unreachable!(),
                    };

                    let Some((out_path_index, out_path_string)) = args_iter.next() else {
                        errors.push((
                            ErrorKind::MustBeFollowedByDirectoryPath(flag),
                            selected_flag_index,
                        ));
                        break 'args;
                    };

                    let out_path = Path::new(out_path_string);
                    if !out_path.is_dir() {
                        errors.push((ErrorKind::MustBeADirectoryPath, out_path_index));
                    }

                    errors.push((ErrorKind::StrayOutputDirectoryOption(flag), selected_flag_index));
                }
                verbosity_flag @ (
                    "--quiet"
                    | "/quiet"
                    | "-q"
                    | "/q"

                    | "--Verbose"
                    | "/Verbose"
                    | "-V"
                    | "/V"
                ) => {
                    let flag = match verbosity_flag {
                        "--quiet" => VerbosityFlag::QuietLong,
                        "/quiet" => VerbosityFlag::QuietLongSlash,
                        "-q" => VerbosityFlag::QuietShort,
                        "/q" => VerbosityFlag::QuietShortSlash,
                        "--Verbose" => VerbosityFlag::VerboseLong,
                        "/Verbose" => VerbosityFlag::VerboseLongSlash,
                        "-V" => VerbosityFlag::VerboseShort,
                        "/V" => VerbosityFlag::VerboseShortSlash,
                        _ => unreachable!(),
                    };

                    errors.push((ErrorKind::StrayVerbosityOption(flag), selected_flag_index));
                }

                "--color-auto"
                | "--color=auto"
                | "/color-auto"
                | "/color=auto"
                | "-c-auto"
                | "-c=auto"
                | "/c-auto"
                | "/c=auto" => color = Color::Auto,

                "--color-always"
                | "--color=always"
                | "/color-always"
                | "/color=always"
                | "-c-always"
                | "-c=always"
                | "/c-always"
                | "/c=always" => color = Color::Always,

                "--color-never"
                | "--color=never"
                | "/color-never"
                | "/color=never"
                | "-c-never"
                | "-c=never"
                | "/c-never"
                | "/c=never" => color = Color::Never,

                color_flag @ ("--color" | "/color" | "-c" | "/c") => {
                    let flag = match color_flag {
                        "--color" => ColorFlag::Long,
                        "/color" => ColorFlag::LongSlash,
                        "-c" => ColorFlag::Short,
                        "/c" => ColorFlag::ShortSlash,
                        _ => unreachable!(),
                    };

                    let Some((selected_color_index, selected_color)) = args_iter.next() else {
                        errors.push((ErrorKind::MissingColorMode(flag), selected_flag_index));
                        break 'args;
                    };

                    color = match selected_color.as_str() {
                        "auto" => Color::Auto,
                        "always" => Color::Always,
                        "never" => Color::Never,
                        _ => {
                            errors.push((ErrorKind::UnrecognizedColorMode, selected_color_index));
                            continue 'args;
                        }
                    };
                }
                _ => errors.push((ErrorKind::Unrecognized, selected_flag_index)),
            }
        }

        color.set(&std::io::stderr());

        if !errors.is_empty() {
            return Err(Error::FromArgs { args, errors });
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

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    MissingColorMode(ColorFlag),
    UnrecognizedColorMode,

    CommandAlreadySelected { current: CommandFlag, previous: CommandFlag },

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
    FromArgs { args: Vec<String>, errors: Vec<(ErrorKind, usize)> },
}

impl Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut args_text = String::new();

        match self {
            Self::EmptyArgs => {
                let pointers_offset = 0_u32;
                let pointers_count = 1_u32;
                let error_message = "missing executable name";
                let error_cause_message = "executable name should always be present";

                let error = MsgWithCauseUnderText {
                    kind: &ERROR,
                    message: &error_message,
                    cause: &error_cause_message,
                    line_text: &args_text,
                    pointers_offset,
                    pointers_count,
                };
                return write!(f, "{error}");
            }
            Self::FromArgs { args, errors } => {
                let mut arg_index = 0;
                let executable_name = &args[arg_index];
                arg_index += 1;
                _ = write!(args_text, "{executable_name}");

                // skipping the executable name
                while arg_index < args.len() {
                    let arg = &args[arg_index];
                    arg_index += 1;
                    _ = write!(args_text, " {arg}");
                }

                let mut error_message = String::new();
                let mut error_cause_message = String::new();

                arg_index = 0;
                let mut pointers_offset = 0;
                for (kind, erroneous_arg_index) in errors {
                    // Note: these two loops are mutually exclusive and avoid extra checking
                    while arg_index < *erroneous_arg_index {
                        let arg = &args[arg_index];
                        arg_index += 1;
                        pointers_offset += arg.len() + 1; // + 1 to account for the space between args
                    }
                    while arg_index > *erroneous_arg_index {
                        arg_index -= 1;
                        let arg = &args[arg_index];
                        pointers_offset -= arg.len() + 1; // + 1 to account for the space between args
                    }

                    let erroneous_arg = &args[arg_index];
                    let pointers_count = match erroneous_arg.len() {
                        0 => 1, // empty arguments will at least get one pointer
                        other => other,
                    };

                    error_message.clear();
                    error_cause_message.clear();
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
                        ErrorKind::CommandAlreadySelected { current, previous } => {
                            _ = write!(error_message, "invalid '{current}' command");
                            _ = write!(
                                error_cause_message,
                                "cannot use '{current}' because '{previous}' was already selected"
                            );
                        }
                        ErrorKind::MustBeFollowedByASourceFilePath(command) => {
                            _ = write!(error_message, "invalid '{command}' command");
                            _ = write!(error_cause_message, "must be followed by a file path");
                        }
                        ErrorKind::MustBeAFilePath => {
                            _ = write!(error_message, "invalid '{erroneous_arg}' path");
                            _ = write!(error_cause_message, "must be a file path");
                        }
                        ErrorKind::MustBeFollowedByDirectoryPath(option) => {
                            _ = write!(error_message, "invalid '{option}' option");
                            _ = write!(error_cause_message, "must be followed by a directory path");
                        }
                        ErrorKind::MustBeADirectoryPath => {
                            _ = write!(error_message, "invalid '{erroneous_arg}' path");
                            _ = write!(error_cause_message, "must be a directory path");
                        }
                        ErrorKind::StrayOutputDirectoryOption(option) => {
                            _ = write!(error_message, "stray '{option}' option");
                            _ = write!(
                                error_cause_message,
                                "can only be used after a '{compile}' or '{run}' command",
                                compile = CommandFlag::Compile,
                                run = CommandFlag::Run,
                            );
                        }
                        ErrorKind::StrayVerbosityOption(option) => {
                            _ = write!(error_message, "stray '{option}' option");
                            _ = write!(
                                error_cause_message,
                                "can only be used after a '{check}', '{compile}' or '{run}' command",
                                check = CommandFlag::Check,
                                compile = CommandFlag::Compile,
                                run = CommandFlag::Run,
                            );
                        }
                        ErrorKind::Unrecognized => {
                            _ = write!(error_message, "unrecognized '{erroneous_arg}' arg");
                            _ = write!(error_cause_message, "unrecognized");
                        }
                    }

                    let error = MsgWithCauseUnderText {
                        kind: &ERROR,
                        message: &error_message,
                        cause: &error_cause_message,
                        line_text: &args_text,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_offset: pointers_offset as offset32,
                        #[expect(clippy::cast_possible_truncation)]
                        pointers_count: pointers_count as offset32,
                    };
                    writeln!(f, "{error}\n")?;
                }
                return Ok(());
            }
        }
    }
}

#[expect(clippy::missing_trait_methods, reason = "using default implementations")]
impl core::error::Error for Error {}
