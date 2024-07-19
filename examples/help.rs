use kaylang::{cli::Color, Help};
use std::path::PathBuf;

fn main() {
    let Some(executable_name) = std::env::args().next() else {
        unreachable!("the os should always put the name of the executable as the first argument");
    };

    let help = Help { color: Color::Auto, executable_name: PathBuf::from(executable_name) };
    println!("{help}");
}
