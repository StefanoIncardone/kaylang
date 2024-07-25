#![cfg(test)]

use kaylang::{
    compiler::{artifacts::Artifacts, Compiler},
    src_file::SrcFile,
    syntax::{ast::Ast, tokenizer::Tokenizer},
    Color, Logger, ASSEMBLING_ERROR, CHECKING, COMPILING, COULD_NOT_RUN_ASSEMBLER,
    COULD_NOT_RUN_EXECUTABLE, COULD_NOT_RUN_LINKER, LINKING_ERROR, RUNNING,
};
use std::{path::PathBuf, process::ExitCode};

#[allow(clippy::panic, clippy::unwrap_used, clippy::panic_in_result_fn)]
#[test]
fn check_project_euler() -> Result<(), ExitCode> {
    let color = Color::Auto;
    color.set(&std::io::stderr());
    color.set(&std::io::stdout());

    let out_path = PathBuf::from("out");
    let src_files = match std::fs::read_dir("examples/project_euler") {
        Ok(files) => files,
        Err(err) => panic!("could not read project_euler folder: {err}"),
    };

    for src_file in src_files {
        let src_path = match src_file {
            Ok(path) => path.path(),
            Err(err) => panic!("could not get path: {err}"),
        };

        let Some(file_name) = src_path.file_name() else {
            continue;
        };

        if file_name == "features_test.kay" || file_name == "fizzbuzz.kay" {
            continue;
        }

        let execution_step = Logger::new();
        Logger::info(&CHECKING, &src_path);

        let src = match SrcFile::load(&src_path) {
            Ok(src) => src,
            Err(err) => {
                eprintln!("{err}");
                return Err(ExitCode::FAILURE);
            }
        };

        let tokens = match Tokenizer::tokenize(&src) {
            Ok(tokens) => tokens,
            Err(errors) => {
                for error in errors {
                    eprintln!("{}\n", error.display(&src));
                }
                return Err(ExitCode::FAILURE);
            }
        };

        let ast = match Ast::build(&src, &tokens) {
            Ok(ast) => ast,
            Err(errors) => {
                for error in errors {
                    eprintln!("{}\n", error.display(&src));
                }
                return Err(ExitCode::FAILURE);
            }
        };

        Logger::info(&COMPILING, &src_path);

        let artifacts = match Artifacts::new_with_out_path(&src, &out_path) {
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

        let running_step = Logger::new();
        Logger::info(&RUNNING, &artifacts.exe_path);

        let mut run_command = artifacts.runner();
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

        let mut lines = stdout.lines();
        let expected = lines.next().unwrap().strip_prefix("expected:").unwrap().trim_start();
        let actual = lines.next().unwrap().strip_prefix("actual:").unwrap().trim_start();

        if !actual.starts_with("# TODO") {
            assert_eq!(expected, actual);
        }
    }

    return Ok(());
}
