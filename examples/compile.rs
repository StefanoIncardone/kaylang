#![allow(clippy::print_stdout, clippy::print_stderr)] // it's a cli tool, it's normal to print to stderr and stdout

use kaylang::{
    compiler::{artifacts::Artifacts, Compiler},
    error,
    src_file::SrcFile,
    syntax::{ast::Parser, tokenizer::Tokenizer},
    Color, Logger, ASSEMBLING, ASSEMBLING_ERROR, BUILDING_AST, CHECKING, COMPILING,
    COULD_NOT_RUN_ASSEMBLER, COULD_NOT_RUN_LINKER, GENERATING_ASM, LINKING, LINKING_ERROR,
    LOADING_SOURCE, SUBSTEP_DONE, TOKENIZATION,
};
use std::{path::PathBuf, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    Color::Auto.set(&std::io::stderr());

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = PathBuf::from("examples/fizzbuzz.kay");

    let execution_step = Logger::new(None);

    Logger::info(&CHECKING, &src_path);
    let checking_sub_step = Logger::new(None);

    let src = {
        let loading_source_sub_step = Logger::new(None);
        let source_loading_result = SrcFile::load(&src_path);
        loading_source_sub_step.sub_step_done(&LOADING_SOURCE);
        match source_loading_result {
            Ok(src) => src,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        }
    };

    let tokens = {
        let tokenization_sub_step = Logger::new(None);
        let tokenizer_result = Tokenizer::tokenize(&src);
        tokenization_sub_step.sub_step_done(&TOKENIZATION);
        match tokenizer_result {
            Ok(tokens) => tokens,
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
        let building_ast_result = Parser::parse(&src, &tokens);
        building_ast_sub_step.sub_step_done(&BUILDING_AST);
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
    let artifacts = match Artifacts::new_with_out_path(&src, &out_path) {
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

    return ExitCode::SUCCESS;
}
