use crate::compiler::ExePath;
use std::process::Command;

#[derive(Debug)]
pub struct Run;

impl Run {
    pub fn run(exe_path: &ExePath) -> Command {
        let exe_path_str = exe_path.inner.to_str().unwrap();
        Command::new(exe_path_str)
    }
}
