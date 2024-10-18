#![allow(clippy::print_stdout, clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    front_end::{
        ast::Ast,
        src_file::SrcFile,
        tokenizer::{TokenizedCode, Tokens},
    },
    Color,
};
use std::{path::PathBuf, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    Color::Auto.set(&std::io::stderr());

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = PathBuf::from("examples/fizzbuzz.kay");

    let src_file = match SrcFile::load(&src_path) {
        Ok(src_file) => src_file,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    };

    let (src, tokens) = {
        let TokenizedCode { result, src } = Tokens::tokenize(&src_file);
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

    let _ast = match Ast::parse(&src, &tokens) {
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
