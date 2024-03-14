use crate::compiler::{AsmPath, ObjPath};
use std::process::Command;

#[derive(Clone, Copy, Debug)]
pub struct Assembler;

impl Assembler {
    pub fn assemble(asm_path: &AsmPath, obj_path: &ObjPath) -> Command {
        let asm_path_str = asm_path.inner.to_str().unwrap();
        let obj_path_str = obj_path.inner.to_str().unwrap();
        let mut assembler_command = Command::new("nasm");
        let _ = assembler_command.args(["-felf64", "-gdwarf", &asm_path_str, "-o", &obj_path_str]);
        assembler_command
    }
}
