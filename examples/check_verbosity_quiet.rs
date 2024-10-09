#![allow(clippy::print_stdout, clippy::print_stderr)] // it's a cli tool, it's normal to print to stderr and stdout

use kaylang::{
    front_end::{ast::Parser, src_file::SrcFile, tokenizer::Tokenizer},
    Color,
};
use std::{path::PathBuf, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    Color::Auto.set(&std::io::stderr());

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = PathBuf::from("examples/fizzbuzz.kay");

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
                eprintln!("{}\n", error.display(&src));
            }
            return ExitCode::FAILURE;
        }
    };

    let _ast = match Parser::parse(&src, &tokens) {
        Ok(ast) => ast,
        Err(errors) => {
            for error in errors {
                eprintln!("{}\n", error.display(&src));
            }
            return ExitCode::FAILURE;
        }
    };

    return ExitCode::SUCCESS;
}
