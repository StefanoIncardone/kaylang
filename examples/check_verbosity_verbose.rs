#![allow(clippy::print_stdout, clippy::print_stderr)] // it's a cli tool, it's normal to print to stderr and stdout

use kaylang::{
    src_file::SrcFile,
    syntax::{ast::Parser, tokenizer::Tokenizer},
    Color, Logger, BUILDING_AST, CHECKING, LOADING_SOURCE, SUBSTEP_DONE, TOKENIZATION,
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

    let _ast = {
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
    execution_step.step_done();
    return ExitCode::SUCCESS;
}
