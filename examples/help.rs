#![expect(clippy::print_stdout, reason = "it's a cli tool")]

use kaylang::{Color, Help};
use std::path::Path;

fn main() {
    let mut env_args = std::env::args();
    let executable_name_option = env_args.next();
    let executable_name = match &executable_name_option {
        Some(executable_name) => Path::new(executable_name),
        None => Help::default_executable_name(),
    };

    let help = Help { color: Color::Auto, executable_name };
    println!("{help}");
}
