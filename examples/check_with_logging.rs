#![expect(clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    Color, Logger,
    front_end::{
        ast::Parser,
        src_file::SrcFile,
        tokenizer::{TokenizedCode, Tokenizer},
    },
    Verbosity,
};
use kaylang::{
    CHECKING, DONE, LOADING_SOURCE, PARSING_AST, SUBSTEP_DONE, TOKENIZATION,
};
use std::{path::Path, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    let color = Color::Auto;
    color.set(&std::io::stderr());

    // controls how much information about compilation steps are reported to the user
    let verbosity = Verbosity::Verbose;

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = Path::new("examples/fizzbuzz.kay");

    let execution_step = Logger::new();

    Logger::info(&CHECKING, src_path, verbosity);
    let checking_sub_step = Logger::new();

    let src_file = {
        let logger = Logger::new();
        let source_loading_result = SrcFile::load(src_path);
        logger.sub_step(&LOADING_SOURCE, None, verbosity);
        match source_loading_result {
            Ok(src_file) => src_file,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            },
        }
    };

    let (src, tokens) = {
        let logger = Logger::new();
        let TokenizedCode { result, src } = Tokenizer::tokenize(&src_file);
        logger.sub_step(&TOKENIZATION, None, verbosity);
        match result {
            Ok(tokens) => (src, tokens),
            Err(errors) => {
                for error in errors {
                    eprintln!("{}\n", error.display(&src));
                }
                return ExitCode::FAILURE;
            },
        }
    };

    let _ast = {
        let logger = Logger::new();
        let building_ast_result = Parser::parse(&src, &tokens);
        logger.sub_step(&PARSING_AST, None, verbosity);
        match building_ast_result {
            Ok(ast) => ast,
            Err(errors) => {
                for error in errors {
                    eprintln!("{}\n", error.display(&src));
                }
                return ExitCode::FAILURE;
            },
        }
    };

    checking_sub_step.sub_step(&DONE, None, verbosity);
    execution_step.step(&DONE, None, verbosity);
    return ExitCode::SUCCESS;
}
