#![expect(clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    back_end::{artifacts::Artifacts, Compiler},
    front_end::{
        ast::Parser,
        src_file::SrcFile,
        tokenizer::{TokenizedCode, Tokenizer},
    },
    Logger, ASSEMBLING_ERROR, CHECKING, COMPILING, COULD_NOT_RUN_ASSEMBLER,
    COULD_NOT_RUN_EXECUTABLE, COULD_NOT_RUN_LINKER, COULD_NOT_WRITE_COMPILED_CODE, DONE,
    LINKING_ERROR, RUNNING,
};
use std::{
    path::Path,
    process::{Command, ExitCode},
};

#[expect(clippy::allow_attributes, reason = "unrealiable")]
#[allow(
    clippy::unwrap_used,
    clippy::panic_in_result_fn,
    dead_code,
    clippy::single_call_fn,
    reason = "it's for testing"
)]
pub(crate) fn check(src_path: &Path) -> Result<(), ExitCode> {
    let execution_step = Logger::new();
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

    let _ast = match Parser::parse(&src, &tokens) {
        Ok(ast) => ast,
        Err(errors) => {
            for error in errors {
                eprintln!("{}\n", error.display(&src));
            }
            return Err(ExitCode::FAILURE);
        }
    };

    execution_step.step(&DONE, None);
    return Ok(());
}

#[expect(clippy::allow_attributes, reason = "unrealiable")]
#[allow(
    clippy::unwrap_used,
    clippy::panic_in_result_fn,
    dead_code,
    clippy::single_call_fn,
    reason = "it's for testing"
)]
pub(crate) fn run(src_path: &Path, out_path: &Path) -> Result<(), ExitCode> {
    let execution_step = Logger::new();
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

    let artifacts = match Artifacts::new(src_path, out_path) {
        Ok(new_artifacts) => new_artifacts,
        Err(err) => {
            eprintln!("{err}");
            return Err(ExitCode::FAILURE);
        }
    };

    let compiled_code = Compiler::compile(&src, &ast);
    if let Err(err) = std::fs::write(&artifacts.asm_path, compiled_code) {
        eprintln!("{COULD_NOT_WRITE_COMPILED_CODE}: {err}");
        return Err(ExitCode::FAILURE);
    }

    let _assembler_status: () = match artifacts.assembler().output() {
        Ok(output) => {
            if !output.status.success() {
                let stderr_out = String::from_utf8_lossy(&output.stderr);
                eprintln!("{ASSEMBLING_ERROR}:\n{stderr_out}");
                return match output.status.code() {
                    #[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                    Some(code) => Err(ExitCode::from(code as u8)),
                    None => Err(ExitCode::FAILURE),
                };
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
                return match output.status.code() {
                    #[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                    Some(code) => Err(ExitCode::from(code as u8)),
                    None => Err(ExitCode::FAILURE),
                };
            }
        }
        Err(err) => {
            eprintln!("{COULD_NOT_RUN_LINKER}: {err}");
            return Err(ExitCode::FAILURE);
        }
    };

    execution_step.step(&DONE, None);

    let running_step = Logger::new();
    let exe_path = Path::new(".").join(&artifacts.exe_path);
    Logger::info(&RUNNING, &exe_path);

    let mut run_command = Command::new(exe_path);
    let run_result = match run_command.output() {
        Ok(output) => output,
        Err(err) => {
            eprintln!("{COULD_NOT_RUN_EXECUTABLE}: {err}");
            return Err(ExitCode::FAILURE);
        }
    };
    running_step.step(&DONE, None);

    let stdout = String::from_utf8_lossy(&run_result.stdout);
    let stderr = String::from_utf8_lossy(&run_result.stderr);

    eprintln!("{stderr}");
    eprintln!("{stdout}");

    if !run_result.status.success() {
        return match run_result.status.code() {
            #[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
            Some(code) => Err(ExitCode::from(code as u8)),
            None => Err(ExitCode::FAILURE),
        };
    }

    let mut example_lines = stdout.lines();
    let expected = example_lines.next().unwrap().strip_prefix("expected:").unwrap().trim_start();
    let actual = example_lines.next().unwrap().strip_prefix("actual:").unwrap().trim_start();

    if !actual.starts_with("# TODO") {
        assert!(expected == actual, "program didn't produce expected output");
    }

    return Ok(());
}
