#![expect(clippy::print_stdout, reason = "it's a cli tool")]

use kaylang::{Color, Version};

fn main() {
    Color::Auto.set(&std::io::stderr());

    let version = Version;
    println!("{version}");
}
