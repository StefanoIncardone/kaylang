use kaylang::{cli::Color, Version};

fn main() {
    let version = Version { color: Color::Auto };
    println!("{version}");
}
