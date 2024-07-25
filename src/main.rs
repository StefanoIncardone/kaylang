#![allow(clippy::print_stdout, clippy::print_stderr)] // it's a cli tool, it's normal to print to stderr and stdout

use kaylang::{
    compiler::{artifacts::Artifacts, Compiler},
    src_file::SrcFile,
    syntax::{ast::Ast, tokenizer::Tokenizer},
    Args, Command, Help, Logger, Version, ASSEMBLING, ASSEMBLING_ERROR, BUILDING_AST, CHECKING,
    COMPILING, COULD_NOT_RUN_ASSEMBLER, COULD_NOT_RUN_EXECUTABLE, COULD_NOT_RUN_LINKER,
    GENERATING_ASM, LINKING, LINKING_ERROR, LOADING_SOURCE, RUNNING, SUBSTEP_DONE, TOKENIZATION,
};
use std::process::ExitCode;

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

    let execution_step = Logger::new();

    Logger::info_with_verbosity(&CHECKING, src_path, verbosity);
    let checking_sub_step = Logger::new();

    let src = {
        let loading_source_sub_step = Logger::new();
        let source_loading_result = SrcFile::load(src_path);
        loading_source_sub_step.sub_step_done_with_verbosity(&LOADING_SOURCE, verbosity);
        match source_loading_result {
            Ok(src) => src,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        }
    };

    let tokens = {
        let tokenization_sub_step = Logger::new();
        let tokenizer_result = Tokenizer::tokenize(&src);
        tokenization_sub_step.sub_step_done_with_verbosity(&TOKENIZATION, verbosity);
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
        let building_ast_sub_step = Logger::new();
        let building_ast_result = Ast::build(&src, &tokens);
        building_ast_sub_step.sub_step_done_with_verbosity(&BUILDING_AST, verbosity);
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
    let compilation_sub_step = Logger::new();

    let artifacts = match out_path {
        None => Artifacts::new(&src),
        Some(path) => match Artifacts::new_with_out_path(&src, path) {
            Ok(artifacts) => artifacts,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        },
    };

    let _compiler_result: () = {
        let generating_asm_sub_step = Logger::new();
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
        let assembling_sub_step = Logger::new();
        let mut assembler_command = artifacts.assembler();
        let assembler_result = assembler_command.output();
        assembling_sub_step.sub_step_done_with_verbosity(&ASSEMBLING, verbosity);
        match assembler_result {
            Ok(output) => {
                if !output.status.success() {
                    let stderr_out = String::from_utf8_lossy(&output.stderr);
                    eprintln!("{ASSEMBLING_ERROR}:\n{stderr_out}");
                    return ExitCode::from(output.status.code().unwrap_or(1) as u8);
                }
            }
            Err(err) => {
                eprintln!("{COULD_NOT_RUN_ASSEMBLER}: {err}");
                return ExitCode::FAILURE;
            }
        }
    };

    let _linker_status: () = {
        let linking_sub_step = Logger::new();
        let mut linker_command = artifacts.linker();
        let linker_result = linker_command.output();
        linking_sub_step.sub_step_done_with_verbosity(&LINKING, verbosity);
        match linker_result {
            Ok(output) => {
                if !output.status.success() {
                    let stderr_out = String::from_utf8_lossy(&output.stderr);
                    eprintln!("{LINKING_ERROR}:\n{stderr_out}");
                    return ExitCode::from(output.status.code().unwrap_or(1) as u8);
                }
            }
            Err(err) => {
                eprintln!("{COULD_NOT_RUN_LINKER}: {err}");
                return ExitCode::FAILURE;
            }
        }
    };

    compilation_sub_step.sub_step_done_with_verbosity(&SUBSTEP_DONE, verbosity);

    let Command::Run { .. } = command else {
        execution_step.step_done_with_verbosity(verbosity);
        return ExitCode::SUCCESS;
    };

    Logger::info_with_verbosity(&RUNNING, &artifacts.exe_path, verbosity);

    let mut run_command = artifacts.runner();
    match run_command.status() {
        Ok(status) => {
            if !status.success() {
                return ExitCode::from(status.code().unwrap_or(1) as u8);
            }
        }
        Err(err) => {
            eprintln!("{COULD_NOT_RUN_EXECUTABLE}: {err}");
            return ExitCode::FAILURE;
        }
    }

    return ExitCode::SUCCESS;
}
