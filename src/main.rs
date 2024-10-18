#![allow(clippy::print_stdout, clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    back_end::{artifacts::Artifacts, Compiler},
    error,
    front_end::{
        ast::Ast,
        src_file::SrcFile,
        tokenizer::{TokenizedCode, Tokens},
    },
    Args, Command, Help, Logger, Version, ASSEMBLING, ASSEMBLING_ERROR, PARSING_AST, CHECKING,
    COMPILING, COULD_NOT_RUN_ASSEMBLER, COULD_NOT_RUN_EXECUTABLE, COULD_NOT_RUN_LINKER,
    GENERATING_ASM, LINKING, LINKING_ERROR, LOADING_SOURCE, RUNNING, SUBSTEP_DONE, TOKENIZATION,
};
use std::{path::PathBuf, process::ExitCode};

// Note: this is aslo an example of how it's possible to create cli tools based on this compiler
fn main() -> ExitCode {
    let Args { color, command } = match Args::try_from(std::env::args()) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    };

    color.set(&std::io::stderr());

    if let Command::Version = command {
        println!("{}", Version { color });
        return ExitCode::SUCCESS;
    }

    if let Command::Help { executable_name } = command {
        println!("{}", Help { executable_name, color });
        return ExitCode::SUCCESS;
    }

    let (Command::Check { src_path, verbosity: verbosity_ref }
    | Command::Compile { src_path, verbosity: verbosity_ref, .. }
    | Command::Run { src_path, verbosity: verbosity_ref, .. }) = &command
    else {
        unreachable!()
    };

    let verbosity = *verbosity_ref;

    let execution_step = Logger::new(None);

    Logger::info_with_verbosity(&CHECKING, src_path, verbosity);
    let checking_sub_step = Logger::new(None);

    let src_file = {
        let loading_source_sub_step = Logger::new(None);
        let source_loading_result = SrcFile::load(src_path);
        loading_source_sub_step.sub_step_done_with_verbosity(&LOADING_SOURCE, verbosity);
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
        tokenization_sub_step.sub_step_done_with_verbosity(&TOKENIZATION, verbosity);
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
        building_ast_sub_step.sub_step_done_with_verbosity(&PARSING_AST, verbosity);
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

    checking_sub_step.sub_step_done_with_verbosity(&SUBSTEP_DONE, verbosity);

    let (Command::Compile { out_path, .. } | Command::Run { out_path, .. }) = &command else {
        execution_step.step_done_with_verbosity(verbosity);
        return ExitCode::SUCCESS;
    };

    Logger::info_with_verbosity(&COMPILING, src_path, verbosity);
    let compilation_sub_step = Logger::new(None);

    let artifacts = match out_path {
        None => Artifacts::new(src_path),
        Some(path) => match Artifacts::new_with_out_path(src_path, path) {
            Ok(artifacts) => artifacts,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        },
    };

    let _compiler_result: () = {
        let generating_asm_sub_step = Logger::new(Some(&artifacts.asm_path));
        let compiler_result = Compiler::compile(&src, &ast, &artifacts);
        generating_asm_sub_step.sub_step_done_with_verbosity(&GENERATING_ASM, verbosity);
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
        assembling_sub_step.sub_step_done_with_verbosity(&ASSEMBLING, verbosity);
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
        linking_sub_step.sub_step_done_with_verbosity(&LINKING, verbosity);
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

    compilation_sub_step.sub_step_done_with_verbosity(&SUBSTEP_DONE, verbosity);
    execution_step.step_done_with_verbosity(verbosity);

    let Command::Run { .. } = command else {
        return ExitCode::SUCCESS;
    };

    let exe_path = PathBuf::from(".").join(&artifacts.exe_path);
    Logger::info_with_verbosity(&RUNNING, &exe_path, verbosity);

    let mut run_command = std::process::Command::new(exe_path);
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
