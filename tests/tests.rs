#![cfg(test)]
#![expect(clippy::panic, clippy::panic_in_result_fn, clippy::print_stderr)]

use kaylang::{
    ASSEMBLING_ERROR, CHECKING, COMPILING, COULD_NOT_RUN_ASSEMBLER, COULD_NOT_RUN_EXECUTABLE, COULD_NOT_RUN_LINKER, COULD_NOT_WRITE_COMPILED_CODE, Color, DONE, LINKING_ERROR, Logger, RUNNING, back_end::{Artifacts, compiler::Compiler}, front_end::{
        ast::Parser,
        src_file::SrcFile,
        syntax_tree as st,
        tokenizer::{TokenizedCode, Tokenizer},
        typed_abstract_syntax_tree as tast,
    }
};
use std::{path::Path, process::{Command, ExitCode}};

#[test]
fn check_project_euler() -> Result<(), ExitCode> {
    let color = Color::Auto;
    color.set(&std::io::stderr());
    color.set(&std::io::stdout());

    let project_euler_path = "examples/project_euler";
    let src_files = match std::fs::read_dir(project_euler_path) {
        Ok(files) => files,
        Err(err) => panic!("could not read '{project_euler_path}' folder: {err}"),
    };

    for src_file in src_files {
        let src_path = match src_file {
            Ok(path) => path.path(),
            Err(err) => panic!("could not get path: {err}"),
        };
        if let Some(extension) = src_path.extension() && extension == "kay" {
            check(&src_path)?;
        }
    }

    return Ok(());
}

#[test]
fn run_project_euler() -> Result<(), ExitCode> {
    let color = Color::Auto;
    color.set(&std::io::stderr());
    color.set(&std::io::stdout());

    let out_path = Path::new("out");
    let project_euler_path = "examples/project_euler";
    let src_files = match std::fs::read_dir(project_euler_path) {
        Ok(files) => files,
        Err(err) => panic!("could not read '{project_euler_path}' folder: {err}"),
    };

    for src_file in src_files {
        let src_path = match src_file {
            Ok(path) => path.path(),
            Err(err) => panic!("could not get path: {err}"),
        };
        if let Some(extension) = src_path.extension() && extension == "kay" {
            run(&src_path, out_path)?;
        }
    }

    return Ok(());
}

#[test]
fn run_escape_characters() -> Result<(), ExitCode> {
    let color = Color::Auto;
    color.set(&std::io::stderr());
    color.set(&std::io::stdout());

    let out_path = Path::new("out");
    let src_path = Path::new("tests/escape_characters.kay");
    return run(src_path, out_path);
}


#[expect(clippy::single_call_fn)]
fn check(src_path: &Path) -> Result<(), ExitCode> {
    let execution_step = Logger::new();
    Logger::info(&CHECKING, src_path);

    let src_file = match SrcFile::load(src_path) {
        Ok(src_file) => src_file,
        Err(err) => {
            eprintln!("{err}");
            return Err(ExitCode::FAILURE);
        },
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
            },
        }
    };

    let syntax_tree = match st::Parser::parse(&src, &tokens) {
        Ok(syntax_tree) => syntax_tree,
        Err(errors) => {
            for error in errors {
                eprintln!("{}\n", error.display(&src));
            }
            return Err(ExitCode::FAILURE);
        },
    };

    #[expect(clippy::let_unit_value)]
    let _typed_syntax_tree = match tast::Parser::parse(&src, &tokens, &syntax_tree) {
        // #[expect(clippy::print_stdout)]
        Ok(_typed_syntax_tree) => {
            // typed_syntax_tree
            // println!("{}", typed_syntax_tree.display(&syntax_tree, &tokens));
        },
        Err(errors) => {
            for error in errors {
                eprintln!("{}\n", error.display(&src));
            }
            // return ExitCode::FAILURE;
        },
    };

    let _ast = match Parser::parse(&src, &tokens) {
        Ok(ast) => ast,
        Err(errors) => {
            for error in errors {
                eprintln!("{}\n", error.display(&src));
            }
            return Err(ExitCode::FAILURE);
        },
    };

    execution_step.step(&DONE, None);
    return Ok(());
}

fn run(src_path: &Path, out_path: &Path) -> Result<(), ExitCode> {
    let execution_step = Logger::new();
    Logger::info(&CHECKING, src_path);

    let src_file = match SrcFile::load(src_path) {
        Ok(src_file) => src_file,
        Err(err) => {
            eprintln!("{err}");
            return Err(ExitCode::FAILURE);
        },
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
            },
        }
    };

    let syntax_tree = match st::Parser::parse(&src, &tokens) {
        Ok(syntax_tree) => syntax_tree,
        Err(errors) => {
            for error in errors {
                eprintln!("{}\n", error.display(&src));
            }
            return Err(ExitCode::FAILURE);
        },
    };

    #[expect(clippy::let_unit_value)]
    let _typed_syntax_tree = match tast::Parser::parse(&src, &tokens, &syntax_tree) {
        // #[expect(clippy::print_stdout)]
        Ok(_typed_syntax_tree) => {
            // typed_syntax_tree
            // println!("{}", typed_syntax_tree.display(&syntax_tree, &tokens));
        },
        Err(errors) => {
            for error in errors {
                eprintln!("{}\n", error.display(&src));
            }
            // return ExitCode::FAILURE;
        },
    };

    let ast = match Parser::parse(&src, &tokens) {
        Ok(ast) => ast,
        Err(errors) => {
            for error in errors {
                eprintln!("{}\n", error.display(&src));
            }
            return Err(ExitCode::FAILURE);
        },
    };

    Logger::info(&COMPILING, src_path);

    let artifacts = match Artifacts::new(src_path, out_path) {
        Ok(new_artifacts) => new_artifacts,
        Err(err) => {
            eprintln!("{err}");
            return Err(ExitCode::FAILURE);
        },
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
                    Some(code) => Err(ExitCode::from(code as u8)),
                    None => Err(ExitCode::FAILURE),
                };
            }
        },
        Err(err) => {
            eprintln!("{COULD_NOT_RUN_ASSEMBLER}: {err}");
            return Err(ExitCode::FAILURE);
        },
    };

    let _linker_status: () = match artifacts.linker().output() {
        Ok(output) => {
            if !output.status.success() {
                let stderr_out = String::from_utf8_lossy(&output.stderr);
                eprintln!("{LINKING_ERROR}:\n{stderr_out}");
                return match output.status.code() {
                    Some(code) => Err(ExitCode::from(code as u8)),
                    None => Err(ExitCode::FAILURE),
                };
            }
        },
        Err(err) => {
            eprintln!("{COULD_NOT_RUN_LINKER}: {err}");
            return Err(ExitCode::FAILURE);
        },
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
        },
    };
    running_step.step(&DONE, None);

    let stdout = unsafe { String::from_utf8_unchecked(run_result.stdout) };
    let stderr = unsafe { String::from_utf8_unchecked(run_result.stderr) };

    eprintln!("{stderr}");
    eprintln!("{stdout}");

    if !run_result.status.success() {
        return match run_result.status.code() {
            Some(code) => Err(ExitCode::from(code as u8)),
            None => Err(ExitCode::FAILURE),
        };
    }

    let mut example_lines = stdout.lines();

    let expected = if let Some(expected_line) = example_lines.next() && let Some(stripped_line) = expected_line.strip_prefix("expected:") {
        stripped_line.trim_start()
    }
    else {
        eprintln!("missing expected output line");
        return Err(ExitCode::FAILURE);
    };

    let actual = if let Some(actual_line) = example_lines.next() && let Some(stripped_line) = actual_line.strip_prefix("actual:") {
        stripped_line.trim_start()
    }
    else {
        eprintln!("missing actual output line");
        return Err(ExitCode::FAILURE);
    };

    if !actual.starts_with("# TODO") && expected != actual {
        eprintln!("program didn't produce expected output");
        return Err(ExitCode::FAILURE);
    }

    return Ok(());
}
