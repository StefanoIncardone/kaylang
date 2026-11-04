#![expect(clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    Color, front_end::{
        ast::Parser, src_file::SrcFile, tokenizer::{TokenizationResult, Tokenizer, Tokens},
    },
};
use std::{path::Path, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    let color = Color::Auto;
    color.set(&std::io::stderr());

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = Path::new("examples/fizzbuzz.kay");

    let src_file = match SrcFile::load(src_path) {
        Ok(src_file) => src_file,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        },
    };

    let tokens = {
        let TokenizationResult { src, tokens, errors } = Tokenizer::tokenize(&src_file);
        if errors.len() > 0 {
            for error in errors {
                eprintln!("{}\n", error.display(&src));
            }
            return ExitCode::FAILURE;
        }
        Tokens { src, tokens }
    };

    let _ast = match Parser::parse(&tokens) {
        Ok(ast) => ast,
        Err(errors) => {
            for error in errors {
                eprintln!("{}\n", error.display(&tokens.src));
            }
            return ExitCode::FAILURE;
        },
    };

    return ExitCode::SUCCESS;
}
