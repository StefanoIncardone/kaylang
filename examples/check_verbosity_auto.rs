use kaylang::{
    src_file::SrcFile,
    syntax::{ast::Ast, tokenizer::Tokenizer},
    Color, Logger, Verbosity, BUILDING_AST, CHECKING, LOADING_SOURCE, SUBSTEP_DONE, TOKENIZATION,
};
use std::{path::PathBuf, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    Color::Auto.set(&std::io::stderr());

    // controls how much information about assembly steps are reported to the user
    let verbosity = Verbosity::Verbose;

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = PathBuf::from("examples/fizzbuzz.kay");

    let execution_step = Logger::new();

    Logger::info_with_verbosity(&CHECKING, &src_path, verbosity);
    let checking_sub_step = Logger::new();

    let src = {
        let loading_source_sub_step = Logger::new();
        let source_loading_result = SrcFile::load(&src_path);
        loading_source_sub_step.sub_step_done_with_verbosity(&LOADING_SOURCE, verbosity);
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
        tokenization_sub_step.sub_step_done_with_verbosity(&TOKENIZATION, verbosity);
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
        let building_ast_sub_step = Logger::new();
        let building_ast_result = Ast::build(&src, &tokens);
        building_ast_sub_step.sub_step_done_with_verbosity(&BUILDING_AST, verbosity);
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

    checking_sub_step.sub_step_done_with_verbosity(&SUBSTEP_DONE, verbosity);
    execution_step.step_done_with_verbosity(verbosity);
    return ExitCode::SUCCESS;
}
