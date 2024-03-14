use crate::compiler::{ExePath, ObjPath};
use std::process::Command;

#[derive(Clone, Copy, Debug)]
pub struct Linker;

impl Linker {
    pub fn link(obj_path: &ObjPath, exe_path: &ExePath) -> Command {
        let obj_path_str = obj_path.inner.to_str().unwrap();
        let exe_path_str = exe_path.inner.to_str().unwrap();
        let mut linker_command = Command::new("ld");
        let _ = linker_command.args([&obj_path_str, "-o", &exe_path_str]);
        linker_command
    }
}
