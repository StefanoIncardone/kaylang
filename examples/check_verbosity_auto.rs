#![allow(clippy::print_stdout, clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    front_end::{
        ast::Parser,
        src_file::SrcFile,
        tokenizer::{TokenizedCode, Tokenizer},
    }, Color, Logger, Verbosity, CHECKING, DONE, LOADING_SOURCE, PARSING_AST, SUBSTEP_DONE, TOKENIZATION
};
use std::{path::PathBuf, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    Color::Auto.set(&std::io::stderr());

    // controls how much information about compilation steps are reported to the user
    let verbosity = Verbosity::Verbose;

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = PathBuf::from("examples/fizzbuzz.kay");

    let execution_step = Logger::new();

    Logger::info_with_verbosity(&CHECKING, &src_path, verbosity);
    let checking_sub_step = Logger::new();

    let src_file = {
        let loading_source_sub_step = Logger::new();
        let source_loading_result = SrcFile::load(&src_path);
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

    let _ast = {
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
    execution_step.step_with_verbosity(&DONE, None, verbosity);
    return ExitCode::SUCCESS;
}
