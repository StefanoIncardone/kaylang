// IDEA(stefano): warn on invalid "-output" (correct: "--output") or "--o" (correct: "-o") or similar

#![warn(clippy::print_stdout, clippy::print_stderr)]

#[cfg(not(target_pointer_width = "64"))]
compile_error!("can only compile for 64bit machines for now");

pub mod back_end;
pub mod color;
pub mod error;
pub mod front_end;

use back_to_front::{
    cli::{Arg, FlagPrefix, FlagSeparator, Mask, bits, mask}, uoffset32,
};
use color::{ansi_flag, AnsiFlag, Bg, Colored, Fg};
use core::fmt::{Display, Write as _};
use error::MsgWithCauseUnderText;
use std::{io::IsTerminal, path::Path, time::Instant};


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

pub(crate) const ERROR: &str = "Error";
pub(crate) const AT: &str = "at";

messages!(
    pub(crate) CAUSE = "Cause", ERR_FG, ERR_BG, ERR_FLAGS;
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

    #[inline]
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

    #[inline]
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

macro_rules! flag {
    ($prefix:ident, $tag:expr) => {
        FlagPrefix::$prefix.combine($tag)
    };
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum FlagLen {
    Short = 0b000_0001,
    Long  = 0b000_0000,
}

impl Mask for FlagLen {
    const MASK: u8 = mask(&[
        Self::Long as u8,
        Self::Short as u8,
    ]);
}


#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum ColorFlag {
    DashDashColor = flag!(DashDash, FlagLen::Long as u8),
    DashC         = flag!(Dash,     FlagLen::Short as u8),
}

impl ColorFlag {
    #[must_use]
    #[inline]
    pub const fn to_str(self) -> &'static str {
        #[rustfmt::skip]
        return match self {
            Self::DashDashColor => "--color",
            Self::DashC         => "-c",
        };
    }
}

impl Display for ColorFlag {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return f.write_str(self.to_str());
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

impl Mask for Color {
    const MASK: u8 = mask(&[
        Self::Auto as u8,
        Self::Always as u8,
        Self::Never as u8,
    ]);
}

impl Color {
    #[must_use]
    #[inline(always)]
    pub const fn to_str(self) -> &'static str {
        #[rustfmt::skip]
        return match self {
            Self::Auto   => "auto",
            Self::Always => "always",
            Self::Never  => "never",
        };
    }
}

impl Display for Color {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return f.write_str(self.to_str());
    }
}

impl Color {
    #[inline]
    pub fn set<I: IsTerminal>(self, sink: &I) {
        #[rustfmt::skip]
        return match self {
            Self::Auto   => Self::set_color_auto(sink),
            Self::Always => Self::set_color_always(),
            Self::Never  => Self::set_color_never(),
        };
    }

    #[inline(always)]
    pub fn set_color_auto<S: IsTerminal>(sink: &S) {
        use crate::color::{print, print_color, print_no_color};
        unsafe {
            print = if sink.is_terminal() { print_color } else { print_no_color };
        }
    }

    #[inline(always)]
    pub const fn set_color_always() {
        use crate::color::{print, print_color};
        unsafe {
            print = print_color;
        }
    }

    #[inline(always)]
    pub const fn set_color_never() {
        use crate::color::{print, print_no_color};
        unsafe {
            print = print_no_color;
        }
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

impl Mask for Language {
    const MASK: u8 = mask(&[
        Self::Kay as u8,
        Self::Asm as u8,
        Self::Obj as u8,
    ]);
}

impl Language {
    #[must_use]
    pub const fn combine(self, tag: u8) -> u8 {
        return bits(&[self as u8, tag << Self::MASK_SHIFT]);
    }

    #[must_use]
    #[inline(always)]
    pub const fn to_str(self) -> &'static str {
        return match self {
            Self::Kay => "kay",
            Self::Asm => "asm",
            Self::Obj => "obj",
        };
    }
}

impl Display for Language {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return f.write_str(self.to_str());
    }
}


#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum LanguageFlag {
    DashDashKay = flag!(DashDash, Language::Kay as u8),
    DashDashAsm = flag!(DashDash, Language::Asm as u8),
    DashDashObj = flag!(DashDash, Language::Obj as u8),
}

impl LanguageFlag {
    #[must_use]
    #[inline]
    pub const fn to_str(self) -> &'static str {
        #[rustfmt::skip]
        return match self {
            Self::DashDashKay => "--kay",
            Self::DashDashAsm => "--asm",
            Self::DashDashObj => "--obj",
        };
    }
}

impl Display for LanguageFlag {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return f.write_str(self.to_str());
    }
}


#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum OutputFlag {
    DashDashOutput = flag!(DashDash, FlagLen::Long as u8),
    DashO          = flag!(Dash,     FlagLen::Short as u8),
}

impl OutputFlag {
    #[must_use]
    #[inline]
    pub const fn to_str(self) -> &'static str {
        #[rustfmt::skip]
        return match self {
            Self::DashDashOutput => "--output",
            Self::DashO          => "-o",
        };
    }
}

impl Display for OutputFlag {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return f.write_str(self.to_str());
    }
}


#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum SrcFlag {
    DashDashSrc = flag!(DashDash, FlagLen::Long as u8),
    DashS       = flag!(Dash,     FlagLen::Short as u8),
}

impl SrcFlag {
    #[must_use]
    #[inline]
    pub const fn to_str(self) -> &'static str {
        #[rustfmt::skip]
        return match self {
            Self::DashDashSrc => "--src",
            Self::DashS       => "-s",
        };
    }
}

impl Display for SrcFlag {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return f.write_str(self.to_str());
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

impl Mask for Verbosity {
    const MASK: u8 = mask(&[
        Self::Normal as u8,
        Self::Quiet as u8,
        Self::Verbose as u8,
    ]);
}

impl Verbosity {
    #[must_use]
    pub const fn combine(self, tag: u8) -> u8 {
        return bits(&[self as u8, tag << Self::MASK_SHIFT]);
    }
}


#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum VerbosityFlag {
    DashDashQuiet   = flag!(DashDash, Verbosity::Quiet.combine(FlagLen::Long as u8)),
    DashQ           = flag!(Dash,     Verbosity::Quiet.combine(FlagLen::Short as u8)),

    DashDashVerbose = flag!(DashDash, Verbosity::Verbose.combine(FlagLen::Long as u8)),
    DashV           = flag!(Dash,     Verbosity::Verbose.combine(FlagLen::Short as u8)),
}

impl VerbosityFlag {
    #[must_use]
    #[inline]
    pub const fn to_str(self) -> &'static str {
        #[rustfmt::skip]
        return match self {
            Self::DashDashQuiet   => "--quiet",
            Self::DashQ           => "-q",

            Self::DashDashVerbose => "--Verbose",
            Self::DashV           => "-V",
        };
    }
}

impl Display for VerbosityFlag {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return f.write_str(self.to_str());
    }
}


#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum CommandFlagKey {
    Help     = 0b0000_0000,
    Question = 0b0000_0001,
    Version  = 0b0000_0010,

    Check    = 0b0000_0100,
    Compile  = 0b0000_0101,
    Run      = 0b0000_0110,
}

impl Mask for CommandFlagKey {
    const MASK: u8 = mask(&[
        Self::Help as u8,
        Self::Question as u8,

        Self::Version as u8,

        Self::Check as u8,
        Self::Compile as u8,
        Self::Run as u8,
    ]);
}

impl CommandFlagKey {
    #[must_use]
    pub const fn combine(self, tag: u8) -> u8 {
        return bits(&[self as u8, tag << Self::MASK_SHIFT]);
    }
}


#[rustfmt::skip]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum CommandFlag {
    Help             = flag!(Empty,    CommandFlagKey::Help.combine(FlagLen::Long as u8)),
    DashDashHelp     = flag!(DashDash, CommandFlagKey::Help.combine(FlagLen::Long as u8)),
    DashH            = flag!(Dash,     CommandFlagKey::Help.combine(FlagLen::Short as u8)),

    Question         = flag!(Empty,    CommandFlagKey::Question.combine(FlagLen::Long as u8)),
    DashDashQuestion = flag!(DashDash, CommandFlagKey::Question.combine(FlagLen::Long as u8)),
    DashQuestion     = flag!(Dash,     CommandFlagKey::Question.combine(FlagLen::Long as u8)),

    Version          = flag!(Empty,    CommandFlagKey::Version.combine(FlagLen::Long as u8)),
    DashDashVersion  = flag!(DashDash, CommandFlagKey::Version.combine(FlagLen::Long as u8)),
    DashV            = flag!(Dash,     CommandFlagKey::Version.combine(FlagLen::Short as u8)),

    Check            = flag!(Empty,    CommandFlagKey::Check as u8),
    Compile          = flag!(Empty,    CommandFlagKey::Compile as u8),
    Run              = flag!(Empty,    CommandFlagKey::Run as u8),
}

impl CommandFlag {
    #[must_use]
    #[inline]
    pub const fn to_str(self) -> &'static str {
        #[rustfmt::skip]
        return match self {
            Self::Help             => "help",
            Self::DashDashHelp     => "--help",
            Self::DashH            => "-h",

            Self::Question         => "?",
            Self::DashDashQuestion => "--?",
            Self::DashQuestion     => "-?",

            Self::Version          => "version",
            Self::DashDashVersion  => "--version",
            Self::DashV            => "-v",

            Self::Check            => "check",
            Self::Compile          => "compile",
            Self::Run              => "run",
        };
    }
}

impl Display for CommandFlag {
    #[inline(always)]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        return f.write_str(self.to_str());
    }
}


#[rustfmt::skip]
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub enum Command<'args> {
    #[default]
    Help,
    Version,

    // TODO(stefano): make composable (repr(u8)) for easy construction
    Check   { src_path: &'args Path, verbosity: Verbosity },
    Compile { src_path: &'args Path, verbosity: Verbosity, out_path: &'args Path, language: Language },
    Run     { src_path: &'args Path, verbosity: Verbosity, out_path: &'args Path, language: Language },
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
pub struct Help<'exe> {
    pub color: Color,
    pub executable_name: &'exe Path,
}

impl Help<'_> {
    #[must_use]
    #[inline(always)]
    pub fn default_executable_name() -> &'static Path {
        return Path::new("kay");
    }
}

impl Default for Help<'_> {
    #[inline]
    fn default() -> Self {
        return Self {
            color: Color::Auto,
            executable_name: Self::default_executable_name(),
        };
    }
}

impl Display for Help<'_> {
    #[expect(non_upper_case_globals)]
    #[rustfmt::skip]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        const fg_value:  Fg = Fg::White;
        const fg_option: Fg = Fg::LightGreen;
        const fg_flag:   Fg = Fg::Blue;
        const bg:        Bg = Bg::Default;
        const flags_value:  ansi_flag = AnsiFlag::Bold as ansi_flag;
        const flags_option: ansi_flag = AnsiFlag::Bold as ansi_flag;
        const flags_flag:   ansi_flag = AnsiFlag::Bold as ansi_flag;

        // IDEA(stefano): make languages flags instead of command variations
        return write!(
            f,
            r"{Version}

{Usage}: {executable_name} [{Command}] [{Options}]
{Arguments}:
    <...>: Required
    [...]: Optional

[{Options}]:
    {__color}, {_c} <{color}>
        Select the output color mode
        (supports '{dash}', '{equals}' or '{colon}' separators: '{_c}{equals}{auto}')

        <{color}>:
            {auto} (default)    Only print colored output if supported
            {always}            Always print colored output, even if not supported
            {never}             Never print colored output

[{Command}]s:
    {help},    {__help},    {_h}, {hq}, {__hq}, {_hq}
        Display this message, and ignore any other command
        (also selected if no other arguments are provided)

    {version}, {__version}, {_v}
        Display the compiler version

    {check}   <{Src}>                     [{Check_Options}]
        Check kay <{Src}> for correctness

    {compile} <{Src}> <{Output}> [{Language}] [{Compile_Options}]
        Compile <{Src}> down to an executable

    {run}     <{Src}> <{Output}> [{Language}] [{Run_Options}]
        Compile <{Src}> and run the generated executable

    [{Check_Options}], [{Compile_Options}], [{Run_Options}]:
        {__quiet},   {_q}    Don't display any compilation information
        {__Verbose}, {_V}    Display extra compilation information

    <{Src}>:
        [{__src}, {_s}] <{src}>
            Used when <{src}> may collide with a flag
            (supports '{equals}' or '{colon}' separators: '{__src}{equals}{src}')

    <{Output}>:
        {__output}, {_o} <{path}>
            Folder at <{path}> to populate with compilation artifacts
            (supports '{equals}' or '{colon}' separators: '{__output}{equals}{out}')

    [{Language}]:
        {__kay} (default)    Treat <{Src}> as a kay source file
        {__asm}              Treat <{Src}> as a assembly source file
        {__obj}              Treat <{Src}> as an object file",

            dash   = FlagSeparator::Dash,
            equals = FlagSeparator::Equals,
            colon  = FlagSeparator::Colon,

            Version         = Version { color: self.color },
            executable_name = self.executable_name.display(),
            Usage           = Colored { text: "Usage",           fg: fg_value,  bg, flags: flags_value },
            Arguments       = Colored { text: "Arguments",       fg: fg_value , bg, flags: flags_value },
            Options         = Colored { text: "Options",         fg: fg_option, bg, flags: flags_option },
            Command         = Colored { text: "Command",         fg: fg_option, bg, flags: flags_option },
            color           = Colored { text: "color",           fg: fg_value,  bg, flags: flags_value },
            Src             = Colored { text: "Src",             fg: fg_option, bg, flags: flags_option },
            src             = Colored { text: "src",             fg: fg_value,  bg, flags: flags_value },
            path            = Colored { text: "path",            fg: fg_value,  bg, flags: flags_value },
            Check_Options   = Colored { text: "Check Options",   fg: fg_option, bg, flags: flags_option },
            Compile_Options = Colored { text: "Compile Options", fg: fg_option, bg, flags: flags_option },
            Run_Options     = Colored { text: "Run Options",     fg: fg_option, bg, flags: flags_option },
            Language        = Colored { text: "Language",        fg: fg_option, bg, flags: flags_option },
            Output          = Colored { text: "Output",          fg: fg_option, bg, flags: flags_option },

            // s   = Colored { text: "*",   fg: fg_flag,  bg, flags: flags_flag },
            out = Colored { text: "out", fg: fg_value, bg, flags: flags_value },

            __color = Colored { text: &ColorFlag::DashDashColor, fg: fg_flag, bg, flags: flags_flag },
            _c      = Colored { text: &ColorFlag::DashC,         fg: fg_flag, bg, flags: flags_flag },
            auto    = Colored { text: &Color::Auto,              fg: fg_flag, bg, flags: flags_flag },
            always  = Colored { text: &Color::Always,            fg: fg_flag, bg, flags: flags_flag },
            never   = Colored { text: &Color::Never,             fg: fg_flag, bg, flags: flags_flag },

            help   = Colored { text: &CommandFlag::Help,             fg: fg_flag, bg, flags: flags_flag },
            __help = Colored { text: &CommandFlag::DashDashHelp,     fg: fg_flag, bg, flags: flags_flag },
            _h     = Colored { text: &CommandFlag::DashH,            fg: fg_flag, bg, flags: flags_flag },
            hq     = Colored { text: &CommandFlag::Question,         fg: fg_flag, bg, flags: flags_flag },
            __hq   = Colored { text: &CommandFlag::DashDashQuestion, fg: fg_flag, bg, flags: flags_flag },
            _hq    = Colored { text: &CommandFlag::DashQuestion,     fg: fg_flag, bg, flags: flags_flag },

            version   = Colored { text: &CommandFlag::Version,         fg: fg_flag, bg, flags: flags_flag },
            __version = Colored { text: &CommandFlag::DashDashVersion, fg: fg_flag, bg, flags: flags_flag },
            _v        = Colored { text: &CommandFlag::DashV,           fg: fg_flag, bg, flags: flags_flag },

            check   = Colored { text: &CommandFlag::Check,   fg: fg_flag, bg, flags: flags_flag },
            compile = Colored { text: &CommandFlag::Compile, fg: fg_flag, bg, flags: flags_flag },
            run     = Colored { text: &CommandFlag::Run,     fg: fg_flag, bg, flags: flags_flag },

            __kay = Colored { text: &LanguageFlag::DashDashKay, fg: fg_flag, bg, flags: flags_flag },
            __asm = Colored { text: &LanguageFlag::DashDashAsm, fg: fg_flag, bg, flags: flags_flag },
            __obj = Colored { text: &LanguageFlag::DashDashObj, fg: fg_flag, bg, flags: flags_flag },

            __output = Colored { text: &OutputFlag::DashDashOutput, fg: fg_flag, bg, flags: flags_flag },
            _o       = Colored { text: &OutputFlag::DashO,          fg: fg_flag, bg, flags: flags_flag },

            __src = Colored { text: &SrcFlag::DashDashSrc, fg: fg_flag, bg, flags: flags_flag },
            _s    = Colored { text: &SrcFlag::DashS,       fg: fg_flag, bg, flags: flags_flag },

            __quiet   = Colored { text: &VerbosityFlag::DashDashQuiet,   fg: fg_flag, bg, flags: flags_flag },
            _q        = Colored { text: &VerbosityFlag::DashQ,           fg: fg_flag, bg, flags: flags_flag },
            __Verbose = Colored { text: &VerbosityFlag::DashDashVerbose, fg: fg_flag, bg, flags: flags_flag },
            _V        = Colored { text: &VerbosityFlag::DashV,           fg: fg_flag, bg, flags: flags_flag },
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

impl Default for Args<'_> {
    fn default() -> Self {
        return Self { command: Ok(Command::default()), color: Color::default() };
    }
}

// IDEA(stefano): use a u16/u32, who needs more than 65K/4G arguments?
pub type ArgIndex = usize;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ArgValue {
    pub arg_index: ArgIndex,
    pub start_of_value_index: u8,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct ParsedArg<'args> {
    index: usize,
    raw: &'args str,
    key: &'args str,
    value: &'args str,
    parsed: Arg,
}


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArgsParser<'args, S: AsRef<str>> {
    args: &'args [S],
    arg_index: usize,

    color: Color,
    verbosity: Verbosity,
    command_flag: Option<(ArgIndex, CommandFlag)>,
    src_path: Option<(ArgValue, Option<SrcFlag>)>,
    out_path: Option<(ArgValue, OutputFlag)>,
    language: Option<(ArgIndex, LanguageFlag)>,
    errors: Vec<Error>,
}

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
            src_path: None,
            out_path: None,
            language: None,
            errors: Vec::new(),
        };

        while let Some(arg) = parser.get_next_arg() {
            match parser.parse_color_flag(&arg) {
                ArgResult::Ok(color) => {
                    parser.color = color;
                    continue;
                },
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            match parser.parse_verbosity_flag(&arg) {
                ArgResult::Ok(verbosity) => {
                    parser.verbosity = verbosity;
                    continue;
                },
                ArgResult::Err => unreachable!(),
                ArgResult::Unrecognized => {},
            }

            match parser.parse_language_flag(&arg) {
                ArgResult::Ok(language) => {
                    parser.language = Some(language);
                    continue;
                },
                ArgResult::Err => unreachable!(),
                ArgResult::Unrecognized => {},
            }

            match parser.parse_help_command(&arg) {
                ArgResult::Ok(command_flag) =>  {
                    parser.set_command(command_flag, arg.index);
                    continue;
                },
                ArgResult::Err => unreachable!(),
                ArgResult::Unrecognized => {},
            }

            match parser.parse_version_command(&arg) {
                ArgResult::Ok(command_flag) => {
                    parser.set_command(command_flag, arg.index);
                    continue;
                },
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            match parser.parse_check_command(&arg) {
                ArgResult::Ok(command_flag) => {
                    parser.set_command(command_flag, arg.index);
                    continue;
                },
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            match parser.parse_compile_command(&arg) {
                ArgResult::Ok(command_flag) => {
                    parser.set_command(command_flag, arg.index);
                    continue;
                },
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            match parser.parse_run_command(&arg) {
                ArgResult::Ok(command_flag) => {
                    parser.set_command(command_flag, arg.index);
                    continue;
                },
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            match parser.parse_out_path(&arg) {
                ArgResult::Ok(out_path) => {
                    parser.out_path = Some(out_path);
                    continue;
                },
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            match parser.parse_src_path(&arg) {
                ArgResult::Ok(src_path) => {
                    parser.src_path = Some(src_path);
                    continue;
                }
                ArgResult::Err => continue,
                ArgResult::Unrecognized => {},
            }

            parser.errors.push(Error::Unrecognized { arg_index: arg.index });
        }

        let command = 'command: {
            use CommandFlag::{
                Help, DashDashHelp, DashH, Question, DashDashQuestion, DashQuestion,
                Version, DashDashVersion, DashV,
                Check, Compile, Run
            };

            let verbosity = parser.verbosity;

            let Some((command_flag_index, command_flag)) = parser.command_flag else {
                if let Some((src_path, src_flag)) = parser.src_path {
                    parser.errors.push(Error::StraySrcPath { arg: src_path, flag: src_flag } );
                }
                else {
                    #[allow(clippy::collapsible_else_if)]
                    if let Some((arg_index, language_flag)) = parser.language {
                        parser.errors.push(Error::StrayLanguageFlag { arg_index, flag: language_flag });
                    }
                }

                if let Some((out_path, output_flag)) = parser.out_path {
                    parser.errors.push(Error::StrayOutputDirectoryFlag { arg: out_path, flag: output_flag });
                }
                break 'command Ok(Command::Help);
            };

            if let Help | DashDashHelp | DashH | Question | DashDashQuestion | DashQuestion = command_flag {
                break 'command Ok(Command::Help);
            }

            if let Version | DashDashVersion | DashV = command_flag {
                break 'command Ok(Command::Version);
            }

            let Some((src_path, _)) = parser.src_path else {
                parser.errors.push(Error::MissingSrcPath {
                    arg_index: command_flag_index,
                    flag: command_flag,
                });
                break 'command Err(());
            };
            let src_path_arg = parser.args[src_path.arg_index].as_ref();
            let src_path_str = &src_path_arg[src_path.start_of_value_index as usize..];
            let src_path_path = Path::new(src_path_str);

            if let Check = command_flag {
                if let Some((out_path, out_path_flag)) = parser.out_path {
                    parser.errors.push(Error::CannotUseOutputDirectoryPathWithCheckCommand {
                        arg_index: out_path.arg_index,
                        flag: out_path_flag,
                    });
                    break 'command Err(());
                }

                if let Some((arg_index, language_flag)) = parser.language {
                    parser.errors.push(Error::CannotUseLanguageFlagWithCheckCommand {
                        arg_index,
                        flag: language_flag,
                    });
                    break 'command Err(());
                }

                break 'command Ok(Command::Check { src_path: src_path_path, verbosity });
            }

            let Some((out_path, _)) = parser.out_path else {
                parser.errors.push(Error::MissingOutputDirectoryPath {
                    arg_index: command_flag_index,
                    flag: command_flag,
                });
                break 'command Err(());
            };

            let out_path_arg = parser.args[out_path.arg_index].as_ref();
            let out_path_str = &out_path_arg[out_path.start_of_value_index as usize..];
            let out_path_path = Path::new(out_path_str);

            let language = match parser.language {
                None => Language::Kay,
                Some((_, language_flag)) => match language_flag {
                    LanguageFlag::DashDashKay => Language::Kay,
                    LanguageFlag::DashDashAsm => Language::Asm,
                    LanguageFlag::DashDashObj => Language::Obj,
                },
            };

            break 'command match command_flag {
                Compile => Ok(Command::Compile {
                    src_path: src_path_path,
                    verbosity,
                    out_path: out_path_path,
                    language,
                }),
                Run => Ok(Command::Run {
                    src_path: src_path_path,
                    verbosity,
                    out_path: out_path_path,
                    language,
                }),
                Help | DashDashHelp | DashH | Question | DashDashQuestion | DashQuestion
                | Version | DashDashVersion | DashV
                | Check => unreachable!(),
            };
        };

        if parser.errors.len() > 0 {
            return Args { color: parser.color, command: Err(parser.errors) };
        }
        let Ok(command_ok) = command else {
            return Args { color: parser.color, command: Err(parser.errors) };
        };
        return Args { color: parser.color, command: Ok(command_ok) };
    }

    #[must_use]
    fn get_next_arg(&mut self) -> Option<ParsedArg<'args>> {
        while self.arg_index < self.args.len() {
            let arg_index = self.arg_index;

            let raw_arg = self.args[arg_index].as_ref();
            self.arg_index += 1;

            let arg_result = Arg::parse(raw_arg);
            let back_to_front::cli::ArgResult::Ok(arg) = arg_result else {
                continue;
            };
            if arg.is_empty() {
                continue;
            }

            let (key, value) = unsafe { arg.key_value(raw_arg) };
            let parsed_arg = ParsedArg { index: arg_index, raw: raw_arg, parsed: arg, key, value };
            return Some(parsed_arg);
        }

        return None;
    }
}

#[allow(clippy::unused_self)]
impl<'args, S: AsRef<str>> ArgsParser<'args, S> {
    fn parse_color_flag(&mut self, arg: &ParsedArg<'args>) -> ArgResult<Color> {
        let color_flag = match arg.key {
            "color" => match arg.parsed.prefix {
                FlagPrefix::DashDash => ColorFlag::DashDashColor,
                FlagPrefix::Empty | FlagPrefix::Dash => return ArgResult::Unrecognized,
            }
            "c" => match arg.parsed.prefix {
                FlagPrefix::Dash => ColorFlag::DashC,
                FlagPrefix::Empty | FlagPrefix::DashDash => return ArgResult::Unrecognized,
            }
            _ => return ArgResult::Unrecognized,
        };

        let (color_str, color_mode_index, start_of_color_index) = match arg.parsed.separator {
            FlagSeparator::Dash | FlagSeparator::Equals | FlagSeparator::Colon => {
                if arg.value.len() == 0 {
                    self.errors.push(Error::MustBeFollowedByColorMode {
                        arg_index: arg.index,
                        flag: color_flag,
                    });
                    return ArgResult::Err;
                }

                let prefix_len = arg.parsed.prefix.to_str().len();
                let key_len = arg.key.len();
                let separator_len = arg.parsed.separator.to_str().len();
                let start_of_color_index = prefix_len + key_len + separator_len;
                (arg.value, arg.index, start_of_color_index)
            },
            FlagSeparator::Empty => {
                let Some(color_arg) = self.get_next_arg() else {
                    self.errors.push(Error::MustBeFollowedByColorMode {
                        arg_index: arg.index,
                        flag: color_flag,
                    });
                    return ArgResult::Err;
                };

                (color_arg.raw, color_arg.index, 0)
            },
        };

        let color = if color_str == Color::Auto.to_str() {
            Color::Auto
        } else if color_str == Color::Always.to_str() {
            Color::Always
        } else if color_str == Color::Never.to_str() {
            Color::Never
        } else {
            self.errors.push(Error::UnrecognizedColorMode(ArgValue {
                arg_index: color_mode_index,
                start_of_value_index: start_of_color_index as u8,
            }));
            return ArgResult::Err;
        };

        return ArgResult::Ok(color);
    }

    fn parse_verbosity_flag(&self, arg: &ParsedArg<'args>) -> ArgResult<Verbosity> {
        use Verbosity::{Quiet, Verbose};
        use FlagPrefix::{Dash, DashDash, Empty};

        let verbosity = match arg.key {
            "quiet" => match arg.parsed.prefix {
                DashDash => Quiet,
                Empty | Dash => return ArgResult::Unrecognized,
            },
            "q" => match arg.parsed.prefix {
                Dash => Quiet,
                Empty | DashDash => return ArgResult::Unrecognized,
            },
            "Verbose" => match arg.parsed.prefix {
                DashDash => Verbose,
                Empty | Dash => return ArgResult::Unrecognized,
            },
            "V" => match arg.parsed.prefix {
                Dash => Verbose,
                Empty | DashDash => return ArgResult::Unrecognized,
            },
            _ => return ArgResult::Unrecognized,
        };

        let FlagSeparator::Empty = arg.parsed.separator else {
            // TODO(stefano): report error
            return ArgResult::Unrecognized;
        };

        return ArgResult::Ok(verbosity)
    }

    fn parse_language_flag(&self, arg: &ParsedArg<'args>) -> ArgResult<(ArgIndex, LanguageFlag)> {
        use LanguageFlag::{DashDashKay, DashDashAsm, DashDashObj};
        use FlagPrefix::{Dash, DashDash, Empty};

        let language_flag = match arg.parsed.prefix {
            DashDash => match arg.key {
                "kay" => DashDashKay,
                "asm" => DashDashAsm,
                "obj" => DashDashObj,
                _ => return ArgResult::Unrecognized,
            },
            Empty | Dash => return ArgResult::Unrecognized,
        };

        let FlagSeparator::Empty = arg.parsed.separator else {
            // TODO(stefano): report error
            return ArgResult::Unrecognized;
        };

        return ArgResult::Ok((arg.index, language_flag));
    }

    fn parse_help_command(&self, arg: &ParsedArg<'args>) -> ArgResult<CommandFlag> {
        use CommandFlag::{Help, DashDashHelp, DashH, Question, DashDashQuestion, DashQuestion};
        use FlagPrefix::{Dash, DashDash, Empty};

        let help_flag = match arg.key {
            "help" => match arg.parsed.prefix {
                Empty => Help,
                DashDash => DashDashHelp,
                Dash => return ArgResult::Unrecognized,
            },
            "h" => match arg.parsed.prefix {
                Dash => DashH,
                Empty | DashDash => return ArgResult::Unrecognized,
            },
            "?" => match arg.parsed.prefix {
                Empty => Question,
                DashDash => DashDashQuestion,
                Dash => DashQuestion,
            },
            _ => return ArgResult::Unrecognized,
        };

        let FlagSeparator::Empty = arg.parsed.separator else {
            // TODO(stefano): report error
            return ArgResult::Unrecognized;
        };

        return ArgResult::Ok(help_flag);
    }

    fn parse_version_command(&self, arg: &ParsedArg<'args>) -> ArgResult<CommandFlag> {
        use CommandFlag::{Version, DashDashVersion, DashV};
        use FlagPrefix::{Dash, DashDash, Empty};

        let version_flag = match arg.key {
            "version" => match arg.parsed.prefix {
                Empty => Version,
                DashDash => DashDashVersion,
                Dash => return ArgResult::Unrecognized,
            },
            "v" => match arg.parsed.prefix {
                Dash => DashV,
                Empty | DashDash => return ArgResult::Unrecognized,
            },
            _ => return ArgResult::Unrecognized,
        };

        let FlagSeparator::Empty = arg.parsed.separator else {
            // TODO(stefano): report error
            return ArgResult::Unrecognized;
        };

        return ArgResult::Ok(version_flag);
    }

    fn parse_check_command(&self, arg: &ParsedArg<'args>) -> ArgResult<CommandFlag> {
        use CommandFlag::Check;
        use FlagPrefix::{Dash, DashDash, Empty};

        let check_flag = match arg.key {
            "check" => match arg.parsed.prefix {
                Empty => Check,
                Dash | DashDash => return ArgResult::Unrecognized,
            }
            _ => return ArgResult::Unrecognized,
        };

        let FlagSeparator::Empty = arg.parsed.separator else {
            // TODO(stefano): report error
            return ArgResult::Unrecognized;
        };

        return ArgResult::Ok(check_flag);
    }

    fn parse_compile_command(&self, arg: &ParsedArg<'args>) -> ArgResult<CommandFlag> {
        use CommandFlag::Compile;
        use FlagPrefix::{Dash, DashDash, Empty};

        let command_flag = match arg.key {
            "compile" => match arg.parsed.prefix {
                Empty => Compile,
                Dash | DashDash => return ArgResult::Unrecognized,
            }
            _ => return ArgResult::Unrecognized,
        };

        let FlagSeparator::Empty = arg.parsed.separator else {
            // TODO(stefano): report error
            return ArgResult::Unrecognized;
        };

        return ArgResult::Ok(command_flag);
    }

    fn parse_run_command(&self, arg: &ParsedArg<'args>) -> ArgResult<CommandFlag> {
        use CommandFlag::Run;
        use FlagPrefix::{Dash, DashDash, Empty};

        let command_flag = match arg.key {
            "run" => match arg.parsed.prefix {
                Empty => Run,
                Dash | DashDash => return ArgResult::Unrecognized,
            }
            _ => return ArgResult::Unrecognized,
        };

        let FlagSeparator::Empty = arg.parsed.separator else {
            // TODO(stefano): report error
            return ArgResult::Unrecognized;
        };

        return ArgResult::Ok(command_flag);
    }

    fn parse_out_path(&mut self, arg: &ParsedArg<'args>) -> ArgResult<(ArgValue, OutputFlag)> {
        use FlagPrefix::{Dash, DashDash, Empty};

        let output_flag = match arg.key {
            "output" => match arg.parsed.prefix {
                DashDash => OutputFlag::DashDashOutput,
                Empty | Dash => return ArgResult::Unrecognized,
            }
            "o" => match arg.parsed.prefix {
                Dash => OutputFlag::DashO,
                Empty | DashDash => return ArgResult::Unrecognized,
            }
            _ => return ArgResult::Unrecognized,
        };

        let out_path = match arg.parsed.separator {
            FlagSeparator::Dash | FlagSeparator::Equals | FlagSeparator::Colon => {
                if arg.value.len() == 0 {
                    self.errors.push(Error::MustBeFollowedByOutputDirectoryPath {
                        arg_index: arg.index,
                        flag: output_flag,
                    });
                    return ArgResult::Err;
                }

                let prefix_len = arg.parsed.prefix.to_str().len();
                let key_len = arg.key.len();
                let separator_len = arg.parsed.separator.to_str().len();
                let start_of_output_index = prefix_len + key_len + separator_len;
                ArgValue { arg_index: arg.index, start_of_value_index: start_of_output_index as u8 }
            },
            FlagSeparator::Empty => {
                let Some(out_arg) = self.get_next_arg() else {
                    self.errors.push(Error::MustBeFollowedByOutputDirectoryPath {
                        arg_index: arg.index,
                        flag: output_flag,
                    });
                    return ArgResult::Err;
                };

                ArgValue { arg_index: out_arg.index, start_of_value_index: 0 }
            },
        };

        let out_path_arg = self.args[out_path.arg_index].as_ref();
        let out_path_str = &out_path_arg[out_path.start_of_value_index as usize..];
        let out_path_path = Path::new(out_path_str);

        if !out_path_path.is_dir() {
            self.errors.push(Error::MustBeADirectoryPath(out_path));
        }

        return ArgResult::Ok((out_path, output_flag));
    }

    fn parse_src_path(&mut self, arg: &ParsedArg<'args>) -> ArgResult<(ArgValue, Option<SrcFlag>)> {
        use FlagPrefix::{Dash, DashDash, Empty};

        let (current_src_path, current_src_flag) = match arg.parsed.prefix {
            Dash | DashDash => {
                let file_flag = match arg.key {
                    "src" => match arg.parsed.prefix {
                        DashDash => SrcFlag::DashDashSrc,
                        Dash => return ArgResult::Unrecognized,
                        Empty => unreachable!(),
                    }
                    "s" => match arg.parsed.prefix {
                        Dash => SrcFlag::DashS,
                        DashDash => return ArgResult::Unrecognized,
                        Empty => unreachable!(),
                    }
                    _ => return ArgResult::Unrecognized,
                };

                match arg.parsed.separator {
                    FlagSeparator::Equals | FlagSeparator::Colon => {
                        if arg.value.len() == 0 {
                            self.errors.push(Error::MustBeFollowedBySrcPath {
                                arg_index: arg.index,
                                flag: file_flag,
                            });
                            return ArgResult::Err;
                        }

                        let prefix_len = arg.parsed.prefix.to_str().len();
                        let key_len = arg.key.len();
                        let separator_len = arg.parsed.separator.to_str().len();
                        let start_of_path_index = prefix_len + key_len + separator_len;
                        (ArgValue { arg_index: arg.index, start_of_value_index: start_of_path_index as u8 }, Some(file_flag))
                    }
                    FlagSeparator::Dash => {
                        // TODO(stefano): report error
                        return ArgResult::Unrecognized;
                    }
                    FlagSeparator::Empty => {
                        let Some(src_arg) = self.get_next_arg() else {
                            self.errors.push(Error::MustBeFollowedBySrcPath {
                                arg_index: arg.index,
                                flag: file_flag,
                            });
                            return ArgResult::Err;
                        };

                        (ArgValue { arg_index: src_arg.index, start_of_value_index: 0 }, Some(file_flag))
                    }
                }
            }
            Empty => (ArgValue { arg_index: arg.index, start_of_value_index: 0 }, None)
        };

        if let Some((previous_src_path, _)) = self.src_path {
            self.errors.push(Error::SrcPathAlreadySelected {
                current: current_src_path,
                previous: previous_src_path,
            });
            return ArgResult::Err;
        }

        let src_path_arg = self.args[current_src_path.arg_index].as_ref();
        let src_path_str = &src_path_arg[current_src_path.start_of_value_index as usize..];
        let src_path_path = Path::new(src_path_str);

        if !src_path_path.is_file() {
            self.errors.push(Error::MustBeAFilePath(current_src_path));
        }

        return ArgResult::Ok((current_src_path, current_src_flag));
    }

    fn set_command(&mut self, command_flag: CommandFlag, command_flag_index: usize) {
        use CommandFlag::{
            Help, DashDashHelp, DashH, Question, DashDashQuestion, DashQuestion,
            Version, DashDashVersion, DashV,
            Check, Compile, Run
        };

        match command_flag {
            Help | DashDashHelp | DashH | Question | DashDashQuestion | DashQuestion => {
                // always setting the help command regardless of where it is found
                self.command_flag = Some((command_flag_index, command_flag));
            }

            Version | DashDashVersion | DashV => match &self.command_flag {
                Some((previous_command_flag_index, previous_command_flag)) => {
                    self.errors.push(Error::CommandAlreadySelected {
                        current_arg_index: command_flag_index,
                        current_flag: command_flag,
                        previous_arg_index: *previous_command_flag_index,
                        previous_flag: *previous_command_flag,
                    });
                },
                None => self.command_flag = Some((command_flag_index, command_flag)),
            }

            Check | Compile | Run => match &self.command_flag {
                Some((
                    _,
                    Help | DashDashHelp | DashH | Question | DashDashQuestion | DashQuestion
                    | Version | DashDashVersion | DashV
                )) => {
                    // make sure the command is properly formatted
                },
                Some((previous_command_flag_index, previous_command_flag @ (Check | Compile | Run))) => {
                    self.errors.push(Error::CommandAlreadySelected {
                        current_arg_index: command_flag_index,
                        current_flag: command_flag,
                        previous_arg_index: *previous_command_flag_index,
                        previous_flag: *previous_command_flag,
                    });
                },
                None => self.command_flag = Some((command_flag_index, command_flag)),
            }
        }
    }
}


#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Error {
    UnrecognizedColorMode(ArgValue),

    MustBeFollowedByColorMode { arg_index: ArgIndex, flag: ColorFlag },
    MustBeFollowedByOutputDirectoryPath { arg_index: ArgIndex, flag: OutputFlag },
    MustBeFollowedBySrcPath { arg_index: ArgIndex, flag: SrcFlag },

    MustBeADirectoryPath(ArgValue),
    MustBeAFilePath(ArgValue),

    MissingOutputDirectoryPath { arg_index: ArgIndex, flag: CommandFlag },
    MissingSrcPath { arg_index: ArgIndex, flag: CommandFlag },

    StrayOutputDirectoryFlag { arg: ArgValue, flag: OutputFlag },
    StraySrcPath { arg: ArgValue, flag: Option<SrcFlag> },
    StrayLanguageFlag { arg_index: ArgIndex, flag: LanguageFlag },

    SrcPathAlreadySelected { previous: ArgValue, current: ArgValue, },
    CannotUseOutputDirectoryPathWithCheckCommand { arg_index: ArgIndex, flag: OutputFlag },
    CannotUseLanguageFlagWithCheckCommand { arg_index: ArgIndex, flag: LanguageFlag },
    CommandAlreadySelected {
        current_arg_index: ArgIndex,
        current_flag: CommandFlag,
        previous_arg_index: ArgIndex,
        previous_flag: CommandFlag,
    },

    Unrecognized { arg_index: ArgIndex },
    MaxLen { arg_index: ArgIndex },
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

        // TODO(stefano): only print the text around the argument to avoid very long error messages
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
        for error in &self.errors {
            let (erroneous_arg_index, pointers_count, arg_value_pointers_offset): (usize, usize, u8) = match error {
                Error::UnrecognizedColorMode(color_mode) => {
                    let arg = self.args[color_mode.arg_index].as_ref();
                    let color = &arg[color_mode.start_of_value_index as usize..];

                    _ = write!(error_message, "unrecognized color mode '{color}'");
                    _ = write!(
                        error_cause_message,
                        "must be one of '{auto}', '{always}' or '{never}'",
                        auto = Color::Auto,
                        always = Color::Always,
                        never = Color::Never,
                    );
                    (color_mode.arg_index, color.len(), color_mode.start_of_value_index)
                },

                Error::MustBeFollowedByColorMode { arg_index: color_flag_index, flag: color_flag } => {
                    let arg = self.args[*color_flag_index].as_ref();
                    _ = write!(error_message, "invalid '{color_flag}' option");
                    _ = write!(
                        error_cause_message,
                        "must be followed by '{auto}', '{always}' or '{never}'",
                        auto = Color::Auto,
                        always = Color::Always,
                        never = Color::Never,
                    );
                    (*color_flag_index, arg.len(), 0)
                },
                Error::MustBeFollowedByOutputDirectoryPath { arg_index: output_flag_index, flag: output_flag } => {
                    let arg = self.args[*output_flag_index].as_ref();
                    _ = write!(error_message, "invalid '{output_flag}' option");
                    _ = write!(error_cause_message, "must be followed by an output directory path");
                    (*output_flag_index, arg.len(), 0)
                },
                Error::MustBeFollowedBySrcPath { arg_index: src_flag_index, flag: src_flag } => {
                    let arg = self.args[*src_flag_index].as_ref();
                    _ = write!(error_message, "invalid '{src_flag}' option");
                    _ = write!(error_cause_message, "must be followed by source file path");
                    (*src_flag_index, arg.len(), 0)
                },

                Error::MustBeADirectoryPath(path) => {
                    let path_arg = &self.args[path.arg_index].as_ref();
                    let path_path = &path_arg[path.start_of_value_index as usize..];

                    _ = write!(error_message, "invalid '{path_path}' path");
                    _ = write!(error_cause_message, "must be a directory path");
                    (path.arg_index, path_path.len(), path.start_of_value_index)
                },
                Error::MustBeAFilePath(path) => {
                    let path_arg = self.args[path.arg_index].as_ref();
                    let path_path = &path_arg[path.start_of_value_index as usize..];

                    _ = write!(error_message, "invalid '{path_path}' path");
                    _ = write!(error_cause_message, "must be a source file path");
                    (path.arg_index, path_path.len(), path.start_of_value_index)
                },

                Error::MissingOutputDirectoryPath { arg_index: flag_index, flag } => {
                    _ = write!(error_message, "invalid '{flag}' command");
                    _ = write!(error_cause_message, "missing output directory path");
                    (*flag_index, flag.to_str().len(), 0)
                },
                Error::MissingSrcPath { arg_index: flag_index, flag } => {
                    _ = write!(error_message, "invalid '{flag}' command");
                    _ = write!(error_cause_message, "missing source file path");
                    (*flag_index, flag.to_str().len(), 0)
                },

                Error::StrayOutputDirectoryFlag { arg, flag } => {
                    _ = write!(error_message, "stray '{flag}' output directory argument");
                    _ = write!(
                        error_cause_message,
                        "can only be used with a '{compile}' or '{run}' command",
                        compile = CommandFlag::Compile,
                        run = CommandFlag::Run,
                    );
                    (arg.arg_index, flag.to_str().len(), 0)
                },
                Error::StraySrcPath { arg, flag } => {
                    _ = write!(
                        error_cause_message,
                        "can only be used with a '{check}', '{compile}' or '{run}' command",
                        check = CommandFlag::Check,
                        compile = CommandFlag::Compile,
                        run = CommandFlag::Run,
                    );

                    match flag {
                        None => {
                            let src_arg = self.args[arg.arg_index].as_ref();
                            _ = write!(error_message, "stray '{src_arg}' source file argument");
                            (arg.arg_index, src_arg.len(), 0)
                        }
                        Some(src_flag) => {
                            _ = write!(error_message, "stray '{src_flag}' source file argument");
                            (arg.arg_index, src_flag.to_str().len(), 0)
                        }
                    }
                },
                Error::StrayLanguageFlag { arg_index: language_arg_index, flag } => {
                    _ = write!(error_message, "stray '{flag}' language flag");
                    _ = write!(error_cause_message, "cannot be used without a source file argument");
                    (*language_arg_index, flag.to_str().len(), 0)
                },

                Error::SrcPathAlreadySelected { previous, current } => {
                    let current_src_arg = self.args[current.arg_index].as_ref();
                    let current_src_path = &current_src_arg[current.start_of_value_index as usize..];

                    let previous_src_arg = self.args[previous.arg_index].as_ref();
                    let previous_src_path = &previous_src_arg[previous.start_of_value_index as usize..];

                    _ = write!(error_message, "repeated '{current_src_path}' source file path argument");
                    _ = write!(
                        error_cause_message,
                        "already selected '{previous_src_path}' source file path"
                    );
                    (current.arg_index, current_src_path.len(), current.start_of_value_index)
                },
                Error::CannotUseOutputDirectoryPathWithCheckCommand { arg_index: out_path_index, flag: output_flag } => {
                    _ = write!(error_message, "invalid '{output_flag}' output directory argument");
                    _ = write!(
                        error_cause_message,
                        "cannot be used with a '{check}' command",
                        check = CommandFlag::Check
                    );
                    (*out_path_index, output_flag.to_str().len(), 0)
                },
                Error::CannotUseLanguageFlagWithCheckCommand { arg_index: language_arg_index, flag: language_flag } => {
                    _ = write!(error_message, "invalid '{language_flag}' language flag");
                    _ = write!(
                        error_cause_message,
                        "cannot be used with a '{check}' command",
                        check = CommandFlag::Check
                    );
                    (*language_arg_index, language_flag.to_str().len(), 0)
                },
                Error::CommandAlreadySelected { current_arg_index, current_flag, previous_flag, .. } => {
                    _ = write!(error_message, "invalid '{current_flag}' command");
                    _ = write!(
                        error_cause_message,
                        "cannot use '{current_flag}' because '{previous_flag}' was already selected"
                    );
                    (*current_arg_index, current_flag.to_str().len(), 0)
                },

                Error::Unrecognized { arg_index: unrecognized_arg_index } => {
                    let arg = self.args[*unrecognized_arg_index].as_ref();
                    _ = write!(error_message, "unrecognized '{arg}' argument");
                    _ = write!(error_cause_message, "unrecognized");
                    (*unrecognized_arg_index, arg.len(), 0)
                },
                Error::MaxLen { arg_index: max_len_arg_index } => {
                    let mut erroneous_arg = self.args[*max_len_arg_index].as_ref();
                    let max_len = erroneous_arg.len().min(8);
                    erroneous_arg = &erroneous_arg[..max_len];

                    _ = write!(error_message, "argument '{erroneous_arg}...' too long");
                    _ = write!(error_cause_message, "over {} characters long", u16::MAX);
                    (*max_len_arg_index, 8, 0)
                },
            };

            // Note: these two loops are mutually exclusive and avoid extra checking
            while arg_index < erroneous_arg_index {
                let arg = &self.args[arg_index].as_ref();
                arg_index += 1;
                pointers_offset += arg.len() + 1; // + 1 to account for the space between args
            }
            while arg_index > erroneous_arg_index {
                arg_index -= 1;
                let arg = &self.args[arg_index].as_ref();
                pointers_offset -= arg.len() + 1; // + 1 to account for the space between args
            }

            let display_pointers_offset = pointers_offset + arg_value_pointers_offset as usize;
            let error_msg = MsgWithCauseUnderText {
                kind: &ERROR,
                message: &error_message,
                cause: &error_cause_message,
                line_text: &args_text,
                #[expect(clippy::cast_possible_truncation)]
                pointers_offset: display_pointers_offset as uoffset32,
                #[expect(clippy::cast_possible_truncation)]
                pointers_count: pointers_count as uoffset32,
            };
            writeln!(f, "{error_msg}\n")?;

            error_message.clear();
            error_cause_message.clear();
        }
        return Ok(());
    }
}

#[expect(clippy::missing_trait_methods)]
impl<S: AsRef<str> + core::fmt::Debug> core::error::Error for Errors<'_, '_, S> {}
