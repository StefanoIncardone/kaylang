#![allow(clippy::print_stdout, clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    back_end::{artifacts::Artifacts, Compiler},
    error,
    front_end::{
        ast::Ast,
        src_file::SrcFile,
        tokenizer::{TokenizedCode, Tokens},
    },
    Color, Logger, ASSEMBLING, ASSEMBLING_ERROR, PARSING_AST, CHECKING, COMPILING,
    COULD_NOT_RUN_ASSEMBLER, COULD_NOT_RUN_EXECUTABLE, COULD_NOT_RUN_LINKER, GENERATING_ASM,
    LINKING, LINKING_ERROR, LOADING_SOURCE, RUNNING, SUBSTEP_DONE, TOKENIZATION,
};
use std::{
    path::PathBuf,
    process::{Command, ExitCode},
};

fn main() -> ExitCode {
    // controls how error messages should be colored
    Color::Auto.set(&std::io::stderr());

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = PathBuf::from("examples/fizzbuzz.kay");

    let execution_step = Logger::new(None);

    Logger::info(&CHECKING, &src_path);
    let checking_sub_step = Logger::new(None);

    let src_file = {
        let loading_source_sub_step = Logger::new(None);
        let source_loading_result = SrcFile::load(&src_path);
        loading_source_sub_step.sub_step_done(&LOADING_SOURCE);
        match source_loading_result {
            Ok(src_file) => src_file,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        }
    };

    let (src, tokens) = {
        let tokenization_sub_step = Logger::new(None);
        let TokenizedCode { result, src } = Tokens::tokenize(&src_file);
        tokenization_sub_step.sub_step_done(&TOKENIZATION);
        match result {
            Ok(tokens) => (src, tokens),
            Err(errors) => {
                for error in errors {
                    eprintln!("{}\n", error.display(&src));
                }
                return ExitCode::FAILURE;
            }
        }
    };

    let ast = {
        let building_ast_sub_step = Logger::new(None);
        let building_ast_result = Ast::parse(&src, &tokens);
        building_ast_sub_step.sub_step_done(&PARSING_AST);
        match building_ast_result {
            Ok(ast) => ast,
            Err(errors) => {
                for error in errors {
                    eprintln!("{}\n", error.display(&src));
                }
                return ExitCode::FAILURE;
            }
        }
    };

    checking_sub_step.sub_step_done(&SUBSTEP_DONE);

    let out_path = PathBuf::from("out");
    let artifacts = match Artifacts::new_with_out_path(&src_path, &out_path) {
        Ok(artifacts) => artifacts,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    };

    Logger::info(&COMPILING, &artifacts.exe_path);
    let compilation_sub_step = Logger::new(None);

    let _compiler_result: () = {
        let generating_asm_sub_step = Logger::new(Some(&artifacts.asm_path));
        let compiler_result = Compiler::compile(&src, &ast, &artifacts);
        generating_asm_sub_step.sub_step_done(&GENERATING_ASM);
        match compiler_result {
            Ok(()) => (),
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        }
    };

    let _assembler_status: () = {
        let assembling_sub_step = Logger::new(Some(&artifacts.obj_path));
        let mut assembler_command = artifacts.assembler();
        let assembler_result = assembler_command.output();
        assembling_sub_step.sub_step_done(&ASSEMBLING);
        match assembler_result {
            Ok(output) => {
                if !output.status.success() {
                    let error = error::Msg {
                        kind: &ASSEMBLING_ERROR,
                        message: &String::from_utf8_lossy(&output.stderr),
                    };
                    eprintln!("{error}");
                    return ExitCode::from(output.status.code().unwrap_or(1) as u8);
                }
            }
            Err(err) => {
                let error = error::Msg { kind: &COULD_NOT_RUN_ASSEMBLER, message: &err };
                eprintln!("{error}");
                return ExitCode::FAILURE;
            }
        }
    };

    let _linker_status: () = {
        let linking_sub_step = Logger::new(Some(&artifacts.exe_path));
        let mut linker_command = artifacts.linker();
        let linker_result = linker_command.output();
        linking_sub_step.sub_step_done(&LINKING);
        match linker_result {
            Ok(output) => {
                if !output.status.success() {
                    let error = error::Msg {
                        kind: &LINKING_ERROR,
                        message: &String::from_utf8_lossy(&output.stderr),
                    };
                    eprintln!("{error}");
                    return ExitCode::from(output.status.code().unwrap_or(1) as u8);
                }
            }
            Err(err) => {
                let error = error::Msg { kind: &COULD_NOT_RUN_LINKER, message: &err };
                eprintln!("{error}");
                return ExitCode::FAILURE;
            }
        }
    };

    compilation_sub_step.sub_step_done(&SUBSTEP_DONE);
    execution_step.step_done();

    let exe_path = PathBuf::from(".").join(&artifacts.exe_path);
    Logger::info(&RUNNING, &exe_path);

    let mut run_command = Command::new(exe_path);
    match run_command.status() {
        Ok(status) => {
            if !status.success() {
                return ExitCode::from(status.code().unwrap_or(1) as u8);
            }
        }
        Err(err) => {
            let error = error::Msg { kind: &COULD_NOT_RUN_EXECUTABLE, message: &err };
            eprintln!("{error}");
            return ExitCode::FAILURE;
        }
    }

    return ExitCode::SUCCESS;
}
