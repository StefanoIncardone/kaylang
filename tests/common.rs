#![allow(clippy::print_stdout, clippy::print_stderr)] // it's a cli tool, it's normal to print to stderr and stdout

use kaylang::{
    back_end::{artifacts::Artifacts, Compiler},
    front_end::{
        ast::Parser,
        src_file::SrcFile,
        tokenizer::{TokenizedCode, Tokenizer},
    },
    Logger, ASSEMBLING_ERROR, CHECKING, COMPILING, COULD_NOT_RUN_ASSEMBLER,
    COULD_NOT_RUN_EXECUTABLE, COULD_NOT_RUN_LINKER, LINKING_ERROR, RUNNING,
};
use std::{
    path::{Path, PathBuf},
    process::{Command, ExitCode},
};

#[allow(
    clippy::panic,
    clippy::unwrap_used,
    clippy::panic_in_result_fn,
    dead_code,
    clippy::single_call_fn
)]
pub(crate) fn run(src_path: &Path, out_path: &Path) -> Result<(), ExitCode> {
    let execution_step = Logger::new(None);
    Logger::info(&CHECKING, src_path);

    let src_file = match SrcFile::load(src_path) {
        Ok(src_file) => src_file,
        Err(err) => {
            eprintln!("{err}");
            return Err(ExitCode::FAILURE);
        }
    };

    let (src, tokens) = {
        let TokenizedCode { result, src } = Tokenizer::tokenize(&src_file);
        match result {
            Ok(tokens) => (src, tokens),
            Err(errors) => {
                for error in errors {
                    eprintln!("{}\n", error.display(&src));
                }
                return Err(ExitCode::FAILURE);
            }
        }
    };

    let ast = match Parser::parse(&src, &tokens) {
        Ok(ast) => ast,
        Err(errors) => {
            for error in errors {
                eprintln!("{}\n", error.display(&src));
            }
            return Err(ExitCode::FAILURE);
        }
    };

    Logger::info(&COMPILING, src_path);

    let artifacts = match Artifacts::new_with_out_path(src_path, out_path) {
        Ok(artifacts) => artifacts,
        Err(err) => {
            eprintln!("{err}");
            return Err(ExitCode::FAILURE);
        }
    };

    let _compiler_result: () = match Compiler::compile(&src, &ast, &artifacts) {
        Ok(()) => (),
        Err(err) => {
            eprintln!("{err}");
            return Err(ExitCode::FAILURE);
        }
    };

    let _assembler_status: () = match artifacts.assembler().output() {
        Ok(output) => {
            if !output.status.success() {
                let stderr_out = String::from_utf8_lossy(&output.stderr);
                eprintln!("{ASSEMBLING_ERROR}:\n{stderr_out}");
                return Err(ExitCode::from(output.status.code().unwrap_or(1) as u8));
            }
        }
        Err(err) => {
            eprintln!("{COULD_NOT_RUN_ASSEMBLER}: {err}");
            return Err(ExitCode::FAILURE);
        }
    };

    let _linker_status: () = match artifacts.linker().output() {
        Ok(output) => {
            if !output.status.success() {
                let stderr_out = String::from_utf8_lossy(&output.stderr);
                eprintln!("{LINKING_ERROR}:\n{stderr_out}");
                return Err(ExitCode::from(output.status.code().unwrap_or(1) as u8));
            }
        }
        Err(err) => {
            eprintln!("{COULD_NOT_RUN_LINKER}: {err}");
            return Err(ExitCode::FAILURE);
        }
    };

    execution_step.step_done();

    let running_step = Logger::new(None);
    let exe_path = PathBuf::from(".").join(&artifacts.exe_path);
    Logger::info(&RUNNING, &exe_path);

    let mut run_command = Command::new(exe_path);
    let run_result = match run_command.output() {
        Ok(output) => output,
        Err(err) => {
            eprintln!("{COULD_NOT_RUN_EXECUTABLE}: {err}");
            return Err(ExitCode::FAILURE);
        }
    };
    running_step.step_done();

    let stdout = String::from_utf8_lossy(&run_result.stdout);
    let stderr = String::from_utf8_lossy(&run_result.stderr);

    eprintln!("{stderr}");
    eprintln!("{stdout}");

    if !run_result.status.success() {
        return Err(ExitCode::from(run_result.status.code().unwrap_or(1) as u8));
    }

    let mut example_lines = stdout.lines();
    let expected = example_lines.next().unwrap().strip_prefix("expected:").unwrap().trim_start();
    let actual = example_lines.next().unwrap().strip_prefix("actual:").unwrap().trim_start();

    if !actual.starts_with("# TODO") {
        assert!(expected == actual, "program didn't produce expected output");
    }

    return Ok(());
}
