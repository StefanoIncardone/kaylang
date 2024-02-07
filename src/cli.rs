use std::{env::Args, fmt::Display, io::IsTerminal, path::PathBuf};

use crate::{
    color::{Bg, Fg, Flag, Options},
    error::CliError,
};

#[allow(non_upper_case_globals)]
pub(crate) static mut display: fn(&str, Options, &mut std::fmt::Formatter<'_>) -> std::fmt::Result = print_color;

pub(crate) fn set_stderr(color: Color) {
    unsafe {
        display = match color {
            Color::Auto => {
                if std::io::stderr().is_terminal() {
                    print_color
                } else {
                    print_no_color
                }
            }
            Color::Always => print_color,
            Color::Never => print_no_color,
        }
    }
}

/// since printing version and help message are the only places where printing to stdoud is
/// performed we are manually checking if stdout (overring stderr coloring modes) is in terminal
/// mode until a way to separately print colored/non-colored output to stdout/stderr is found
pub(crate) fn set_stdout(color: Color) {
    unsafe {
        display = match color {
            Color::Auto => {
                if std::io::stdout().is_terminal() {
                    print_color
                } else {
                    print_no_color
                }
            }
            Color::Always => print_color,
            Color::Never => print_no_color,
        }
    }
}

fn print_no_color(text: &str, _: Options, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return text.fmt(f);
}

fn print_color(text: &str, opt: Options, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut codes = String::with_capacity(15);

    if opt.fg != Fg::Default {
        codes += &format!("{};", opt.fg as u8);
    }
    if opt.bg != Bg::Default {
        codes += &format!("{};", opt.bg as u8);
    }
    if opt.flags & Flag::Bold != 0 {
        codes += "1;";
    }
    if opt.flags & Flag::Underline != 0 {
        codes += "4;";
    }
    if opt.flags & Flag::NoUnderline != 0 {
        codes += "24;";
    }
    if opt.flags & Flag::ReverseText != 0 {
        codes += "7;";
    }
    if opt.flags & Flag::PositiveText != 0 {
        codes += "27;";
    }

    return if codes.is_empty() {
        text.fmt(f)
    } else {
        let _last_semicolon = codes.pop();

        write!(f, "\x1b[{}m", codes)?;
        text.fmt(f)?;
        write!(f, "\x1b[0m")
    };
}

#[derive(Debug, Default, Clone, Copy)]
pub enum Color {
    #[default]
    Auto,
    Always,
    Never,
}

impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Always => write!(f, "always"),
            Self::Auto => write!(f, "auto"),
            Self::Never => write!(f, "never"),
        };
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum Verbosity {
    #[default]
    Normal,
    Quiet,
    Verbose,
}

impl Display for Verbosity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Quiet => write!(f, "quiet"),
            Self::Normal => write!(f, "normal"),
            Self::Verbose => write!(f, "verbose"),
        };
    }
}

#[derive(Debug, Default, Clone)]
pub enum RunMode {
    #[default]
    Help,
    Version,
    Check {
        src_path: PathBuf,
    },
    Compile {
        src_path: PathBuf,
        out_path: Option<PathBuf>,
        run: bool,
    },
}

impl Display for RunMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Help => write!(f, "help"),
            Self::Version => write!(f, "version"),
            Self::Check { src_path } => write!(f, "check {}", src_path.display()),
            Self::Compile { src_path, out_path, run } => {
                if *run {
                    write!(f, "run ")?;
                } else {
                    write!(f, "compile ")?;
                }

                write!(f, "{} ", src_path.display())?;

                if let Some(path) = out_path {
                    write!(f, "-o {}", path.display())?
                }

                Ok(())
            }
        };
    }
}

#[derive(Debug, Default, Clone)]
pub struct KayArgs {
    pub color: Option<Color>,
    pub verbosity: Option<Verbosity>,
    pub run_mode: Option<RunMode>,
}

impl TryFrom<Vec<String>> for KayArgs {
    type Error = CliError;

    fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
        let args_iter = args.iter().peekable();

        let mut args = args_iter.clone();
        let _ = args.next(); // skipping the name of this executable

        set_stderr(Color::Auto);
        let mut color_mode: Option<Color> = None;

        while let Some(arg) = args.next() {
            if arg == "-c" || arg == "--color" {
                if let Some(mode) = color_mode {
                    return Err(CliError { msg: format!("'{}' color mode already selected", mode).into() });
                }

                let Some(mode) = args.next() else {
                    return Err(CliError { msg: "expected color mode".into() });
                };

                color_mode = match mode.as_str() {
                    "auto" => Some(Color::Auto),
                    "always" => Some(Color::Always),
                    "never" => Some(Color::Never),
                    _ => return Err(CliError { msg: "unrecognized color mode".into() }),
                };
            }
        }

        let color = color_mode.unwrap_or_default();
        set_stderr(color);

        let mut verbosity: Option<Verbosity> = None;
        let mut run_mode: Option<RunMode> = None;

        args = args_iter.clone();
        let _ = args.next(); // skipping the name of this executable

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-h" | "--help" => match run_mode {
                    Some(RunMode::Help) => {
                        return Err(CliError { msg: format!("'{}' help command already selected", arg).into() })
                    }
                    Some(RunMode::Version) => {
                        return Err(CliError { msg: "help and version commands cannot be used together".into() })
                    }
                    _ => run_mode = Some(RunMode::Help),
                },
                "-v" | "--version" => match run_mode {
                    Some(RunMode::Version) => {
                        return Err(CliError { msg: format!("'{}' version command already selected", arg).into() })
                    }
                    Some(RunMode::Help) => {
                        return Err(CliError { msg: "help and version commands cannot be used together".into() })
                    }
                    _ => run_mode = Some(RunMode::Version),
                },
                "check" | "compile" | "run" => {
                    if let Some(RunMode::Check { .. } | RunMode::Compile { .. }) = run_mode {
                        return Err(CliError { msg: format!("'{}' run mode already selected", arg).into() });
                    }

                    let src_path: PathBuf = match args.next() {
                        Some(path) => path.into(),
                        None => {
                            return Err(CliError { msg: format!("missing source file path for '{}' mode", arg).into() })
                        }
                    };

                    let mode = match arg.as_str() {
                        "check" => RunMode::Check { src_path },
                        "compile" | "run" => {
                            let mut out: Option<PathBuf> = None;

                            if let Some(out_flag) = args.peek() {
                                if *out_flag == "-o" || *out_flag == "--output" {
                                    let _ = args.next();

                                    out = match args.next() {
                                        Some(path) => Some(path.into()),
                                        None => return Err(CliError { msg: "missing output folder path".into() }),
                                    };
                                }
                            }

                            match arg.as_str() {
                                "compile" => RunMode::Compile { src_path, out_path: out, run: false },
                                "run" => RunMode::Compile { src_path, out_path: out, run: true },
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    };

                    if let Some(RunMode::Help | RunMode::Version) = run_mode {
                        // this is just to make sure that run modes commands are properly
                        // formatted, so we do nothing in the case where the -h, --help, -v
                        // or --version command was already selected
                    } else {
                        run_mode = Some(mode);
                    }
                }
                "-q" | "--quiet" | "-V" | "--verbose" => {
                    if let Some(mode) = verbosity {
                        return Err(CliError { msg: format!("'{}' verbosity mode already selected", mode).into() });
                    }

                    verbosity = match arg.as_str() {
                        "-q" | "--quiet" => Some(Verbosity::Quiet),
                        "-V" | "--verbose" => Some(Verbosity::Verbose),
                        _ => unreachable!(),
                    };
                }
                "-o" | "--output" => {
                    let Some(_) = args.next() else {
                        return Err(CliError { msg: "missing output folder path".into() });
                    };

                    return Err(CliError {
                        msg: "output folder option can only be used after a 'compile' or 'run' command".into(),
                    });
                }
                "-c" | "--color" => {
                    let _ = args.next();
                }
                _ => return Err(CliError { msg: format!("unrecognized option '{}'", arg).into() }),
            }
        }

        return Ok(Self { color: Some(color), verbosity, run_mode });
    }
}

impl TryFrom<Args> for KayArgs {
    type Error = CliError;

    fn try_from(args: Args) -> Result<Self, Self::Error> {
        return Self::try_from(args.collect::<Vec<String>>());
    }
}
