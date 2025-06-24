#![expect(clippy::print_stdout, reason = "it's a cli tool")]

use kaylang::{Color, Help};
use std::path::PathBuf;

fn main() {
    let mut env_args = std::env::args();
    let executable_name = match env_args.next() {
        Some(executable_name) => PathBuf::from(executable_name),
        None => Help::default().executable_name,
    };

    let help = Help { color: Color::Auto, executable_name };
    println!("{help}");
}
