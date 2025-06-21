#![expect(clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    back_end::{artifacts::Artifacts, Compiler},
    error,
    front_end::{
        ast::Parser,
        src_file::SrcFile,
        tokenizer::{TokenizedCode, Tokenizer},
    },
    Color, Language, Logger, ASSEMBLING, ASSEMBLING_ERROR, CHECKING, COMPILING,
    COULD_NOT_RUN_ASSEMBLER, COULD_NOT_RUN_LINKER, COULD_NOT_WRITE_COMPILED_CODE, DONE,
    GENERATING_ASM, LINKING, LINKING_ERROR, LOADING_SOURCE, PARSING_AST, SUBSTEP_DONE,
    TOKENIZATION,
};
use std::{path::Path, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    Color::Auto.set(&std::io::stderr());

    // simulating language selection flags `--kay`, `--asm` and `--obj`
    let language = Language::Kay;

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = {
        let src_path = match language {
            Language::Kay => "examples/fizzbuzz.kay",
            Language::Asm => "examples/fizzbuzz.asm",
            Language::Obj => "examples/fizzbuzz.o",
        };
        Path::new(src_path)
    };
    let out_path = Path::new("examples/out");

    let artifacts: Artifacts;
    let compilation_sub_step: Logger;
    let execution_step = Logger::new();

    Logger::info(&CHECKING, src_path);
    let checking_sub_step = Logger::new();

    if let Language::Kay = language {
        let src_file = {
            let loading_source_sub_step = Logger::new();
            let source_loading_result = SrcFile::load(src_path);
            loading_source_sub_step.sub_step(&LOADING_SOURCE, None);
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
            tokenization_sub_step.sub_step(&TOKENIZATION, None);
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
            building_ast_sub_step.sub_step(&PARSING_AST, None);
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

        checking_sub_step.sub_step(&SUBSTEP_DONE, None);

        artifacts = match Artifacts::new(src_path, out_path) {
            Ok(new_artifacts) => new_artifacts,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        };

        Logger::info(&COMPILING, src_path);
        compilation_sub_step = Logger::new();

        let _compiler_result: () = {
            let generating_asm_sub_step = Logger::new();
            let compiled_code = Compiler::compile(&src, &ast);
            generating_asm_sub_step.sub_step(&GENERATING_ASM, Some(&artifacts.asm_path));
            if let Err(err) = std::fs::write(&artifacts.asm_path, compiled_code) {
                let error = error::Msg { kind: &COULD_NOT_WRITE_COMPILED_CODE, message: &err };
                eprintln!("{error}");
                return ExitCode::FAILURE;
            }
        };
    } else {
        artifacts = match Artifacts::new(src_path, out_path) {
            Ok(new_artifacts) => new_artifacts,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        };

        Logger::info(&COMPILING, src_path);
        compilation_sub_step = Logger::new();
    }

    if let Language::Kay | Language::Asm = language {
        let _assembler_status: () = {
            let assembling_sub_step = Logger::new();
            let mut assembler_command = artifacts.assembler();
            let assembler_result = assembler_command.output();
            assembling_sub_step.sub_step(&ASSEMBLING, Some(&artifacts.obj_path));
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
    }

    let _linker_status: () = {
        let linking_sub_step = Logger::new();
        let mut linker_command = artifacts.linker();
        let linker_result = linker_command.output();
        linking_sub_step.sub_step(&LINKING, Some(&artifacts.exe_path));
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

    compilation_sub_step.sub_step(&SUBSTEP_DONE, None);
    execution_step.step(&DONE, None);

    return ExitCode::SUCCESS;
}
