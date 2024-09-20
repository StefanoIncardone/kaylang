#![allow(clippy::print_stdout, clippy::print_stderr)] // it's a cli tool, it's normal to print to stderr and stdout

use kaylang::{Color, Version};

fn main() {
    let version = Version { color: Color::Auto };
    println!("{version}");
}
