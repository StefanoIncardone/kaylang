use kaylang::{
    cli::Color,
    src_file::SrcFile,
    syntax::{ast::Ast, tokenizer::Tokenizer},
    Logger, CHECKING,
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

    let src = match SrcFile::load(&src_path) {
        Ok(src) => src,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    };

    let tokens = match Tokenizer::tokenize(&src) {
        Ok(tokens) => tokens,
        Err(errors) => {
            for error in errors {
                eprintln!("{error}\n");
            }
            return ExitCode::FAILURE;
        }
    };

    let _ast = match Ast::build(&src, &tokens) {
        Ok(ast) => ast,
        Err(errors) => {
            for error in errors {
                eprintln!("{error}\n");
            }
            return ExitCode::FAILURE;
        }
    };

    execution_step.step_done();
    return ExitCode::SUCCESS;
}
