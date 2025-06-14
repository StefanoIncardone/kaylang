#![expect(clippy::print_stdout, clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    back_end::{artifacts::Artifacts, Compiler},
    error,
    front_end::{
        ast::Parser,
        src_file::SrcFile,
        tokenizer::{TokenizedCode, Tokenizer},
    },
    Args, Command, Help, Logger, Version, ASSEMBLING, ASSEMBLING_ERROR, CHECKING, COMPILING,
    COULD_NOT_RUN_ASSEMBLER, COULD_NOT_RUN_EXECUTABLE, COULD_NOT_RUN_LINKER,
    COULD_NOT_WRITE_COMPILED_CODE, DONE, GENERATING_ASM, LINKING, LINKING_ERROR, LOADING_SOURCE,
    PARSING_AST, RUNNING, SUBSTEP_DONE, TOKENIZATION,
};
use std::{path::PathBuf, process::ExitCode};

// Note: this is aslo an example of how it's possible to create cli tools based on this compiler
fn main() -> ExitCode {
    let Args { color, command } = match Args::try_from(std::env::args()) {
        Ok(args) => args,
        Err(err) => {
            eprint!("{err}");
            return ExitCode::FAILURE;
        }
    };

    color.set(&std::io::stderr());

    if let Command::Version = command {
        println!("{}", Version { color });
        return ExitCode::SUCCESS;
    }

    if let Command::Help { executable_name } = command {
        println!("{}", Help { color, executable_name });
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

    let src_file = {
        let loading_source_sub_step = Logger::new();
        let source_loading_result = SrcFile::load(src_path);
        loading_source_sub_step.sub_step_with_verbosity(&LOADING_SOURCE, None, verbosity);
        match source_loading_result {
            Ok(src_file) => src_file,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        }
    };

    let (src, tokens) = {
        let tokenization_sub_step = Logger::new();
        let TokenizedCode { result, src } = Tokenizer::tokenize(&src_file);
        tokenization_sub_step.sub_step_with_verbosity(&TOKENIZATION, None, verbosity);
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
        let building_ast_sub_step = Logger::new();
        let building_ast_result = Parser::parse(&src, &tokens);
        building_ast_sub_step.sub_step_with_verbosity(&PARSING_AST, None, verbosity);
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

    checking_sub_step.sub_step_with_verbosity(&SUBSTEP_DONE, None, verbosity);

    let (Command::Compile { out_path, .. } | Command::Run { out_path, .. }) = &command else {
        execution_step.step_with_verbosity(&DONE, None, verbosity);
        return ExitCode::SUCCESS;
    };

    Logger::info_with_verbosity(&COMPILING, src_path, verbosity);
    let compilation_sub_step = Logger::new();

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
        let generating_asm_sub_step = Logger::new();
        let compiled_code = Compiler::compile(&src, &ast);
        generating_asm_sub_step.sub_step_with_verbosity(
            &GENERATING_ASM,
            Some(&artifacts.asm_path),
            verbosity,
        );
        if let Err(err) = std::fs::write(&artifacts.asm_path, compiled_code) {
            let error = error::Msg { kind: &COULD_NOT_WRITE_COMPILED_CODE, message: &err };
            eprintln!("{error}");
            return ExitCode::FAILURE;
        }
    };

    let _assembler_status: () = {
        let assembling_sub_step = Logger::new();
        let mut assembler_command = artifacts.assembler();
        let assembler_result = assembler_command.output();
        assembling_sub_step.sub_step_with_verbosity(
            &ASSEMBLING,
            Some(&artifacts.obj_path),
            verbosity,
        );
        match assembler_result {
            Ok(output) => {
                if !output.status.success() {
                    let error = error::Msg {
                        kind: &ASSEMBLING_ERROR,
                        message: &String::from_utf8_lossy(&output.stderr),
                    };
                    eprintln!("{error}");
                    return match output.status.code() {
                        #[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                        Some(code) => ExitCode::from(code as u8),
                        None => ExitCode::FAILURE,
                    };
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
        let linking_sub_step = Logger::new();
        let mut linker_command = artifacts.linker();
        let linker_result = linker_command.output();
        linking_sub_step.sub_step_with_verbosity(&LINKING, Some(&artifacts.exe_path), verbosity);
        match linker_result {
            Ok(output) => {
                if !output.status.success() {
                    let error = error::Msg {
                        kind: &LINKING_ERROR,
                        message: &String::from_utf8_lossy(&output.stderr),
                    };
                    eprintln!("{error}");
                    return match output.status.code() {
                        #[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                        Some(code) => ExitCode::from(code as u8),
                        None => ExitCode::FAILURE,
                    };
                }
            }
            Err(err) => {
                let error = error::Msg { kind: &COULD_NOT_RUN_LINKER, message: &err };
                eprintln!("{error}");
                return ExitCode::FAILURE;
            }
        }
    };

    compilation_sub_step.sub_step_with_verbosity(&SUBSTEP_DONE, None, verbosity);
    execution_step.step_with_verbosity(&DONE, None, verbosity);

    let Command::Run { .. } = command else {
        return ExitCode::SUCCESS;
    };

    let exe_path = PathBuf::from(".").join(&artifacts.exe_path);
    Logger::info_with_verbosity(&RUNNING, &exe_path, verbosity);

    let mut run_command = std::process::Command::new(exe_path);
    match run_command.status() {
        Ok(status) => {
            if !status.success() {
                return match status.code() {
                    #[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                    Some(code) => ExitCode::from(code as u8),
                    None => ExitCode::FAILURE,
                };
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
