use kaylang::{
    artifacts::Artifacts,
    compiler::Compiler,
    src_file::SrcFile,
    syntax::{ast::Ast, tokenizer::Tokenizer},
    Color, Logger, ASSEMBLING, BUILDING_AST, CHECKING, COMPILING, GENERATING_ASM, LINKING,
    LOADING_SOURCE, SUBSTEP_DONE, TOKENIZATION,
};
use std::{path::PathBuf, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    Color::Auto.set(&std::io::stderr());

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = PathBuf::from("examples/fizzbuzz.kay");

    let execution_step = Logger::new();

    Logger::info(&CHECKING, &src_path);
    let checking_sub_step = Logger::new();

    let src = {
        let loading_source_sub_step = Logger::new();
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
        let tokenization_sub_step = Logger::new();
        let tokenizer_result = Tokenizer::tokenize(&src);
        tokenization_sub_step.sub_step_done(&TOKENIZATION);
        match tokenizer_result {
            Ok(tokens) => tokens,
            Err(errors) => {
                for error in errors {
                    eprintln!("{error}\n");
                }
                return ExitCode::FAILURE;
            }
        }
    };

    let ast = {
        let building_ast_sub_step = Logger::new();
        let building_ast_result = Ast::build(&src, &tokens);
        building_ast_sub_step.sub_step_done(&BUILDING_AST);
        match building_ast_result {
            Ok(ast) => ast,
            Err(errors) => {
                for error in errors {
                    eprintln!("{error}\n");
                }
                return ExitCode::FAILURE;
            }
        }
    };

    checking_sub_step.sub_step_done(&SUBSTEP_DONE);

    let out_path = PathBuf::from("out");
    let artifacts = match Artifacts::new(&src, Some(&out_path)) {
        Ok(artifacts) => artifacts,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    };

    Logger::info(&COMPILING, &artifacts.exe_path);
    let compilation_sub_step = Logger::new();

    let _compiler_result: () = {
        let generating_asm_sub_step = Logger::new();
        let compiler_result = Compiler::compile(&src, &artifacts, &ast);
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
        let assembling_sub_step = Logger::new();
        let mut assembler_command = artifacts.assembler();
        let assembler_result = assembler_command.status();
        assembling_sub_step.sub_step_done(&ASSEMBLING);
        match assembler_result {
            Ok(status) => {
                if !status.success() {
                    return ExitCode::from(status.code().unwrap_or(1) as u8);
                }
            }
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        }
    };

    let _linker_status: () = {
        let linking_sub_step = Logger::new();
        let mut linker_command = artifacts.linker();
        let linker_result = linker_command.status();
        linking_sub_step.sub_step_done(&LINKING);
        match linker_result {
            Ok(status) => {
                if !status.success() {
                    return ExitCode::from(status.code().unwrap_or(1) as u8);
                }
            }
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        }
    };

    compilation_sub_step.sub_step_done(&SUBSTEP_DONE);
    execution_step.step_done();

    return ExitCode::SUCCESS;
}
