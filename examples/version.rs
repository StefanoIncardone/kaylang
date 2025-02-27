#![allow(clippy::print_stdout, clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{Color, Version};

fn main() {
    let version = Version { color: Color::Auto };
    println!("{version}");
}
