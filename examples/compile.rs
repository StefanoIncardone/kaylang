#![expect(clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    Color, Language, Logger, Verbosity, code_generation::{Artifacts, Platform, nasm::compiler_ast::Compiler}, error::MsgSimple, front_end::{
        ast::Parser,
        src_file::SrcFile,
        tokenizer::{TokenizationResult, Tokenizer},
    },
};
use kaylang::{
    ASSEMBLING, ASSEMBLING_ERROR, PARSING, COMPILING, COULD_NOT_RUN_ASSEMBLER, COULD_NOT_RUN_LINKER,
    COULD_NOT_WRITE_COMPILED_CODE,
    DONE, GENERATING_ASM, LINKING, LINKING_ERROR, LOADING_SOURCE,
    PARSING_AST, TOKENIZATION,
};
use std::{path::Path, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    let color = Color::Auto;
    color.set(&std::io::stderr());

    // simulating language selection flags `--kay`, `--asm` and `--obj`
    let language = Language::Kay;

    // controls how much information about compilation steps are reported to the user
    let verbosity = Verbosity::Verbose;

    // control the platform to target for code generation
    let platform = Platform::Auto;

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
    let out_path = Path::new("out");

    let artifacts: Artifacts;
    let compilation_sub_step: Logger;
    let execution_step = Logger::new();

    Logger::info(&PARSING, src_path, verbosity);
    let checking_sub_step = Logger::new();

    if let Language::Kay = language {
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
            let TokenizationResult { result, src } = Tokenizer::tokenize(&src_file);
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

        let ast = {
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

        let extensions = platform.extensions();
        artifacts = match Artifacts::new(src_path, out_path, &extensions) {
            Ok(new_artifacts) => new_artifacts,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            },
        };

        Logger::info(&COMPILING, src_path, verbosity);
        compilation_sub_step = Logger::new();

        let _compiler_result: () = {
            let logger = Logger::new();
            let compiled_code = Compiler::compile(&src, &ast);
            logger.sub_step(&GENERATING_ASM, Some(&artifacts.asm_path), verbosity);
            if let Err(err) = std::fs::write(&artifacts.asm_path, compiled_code) {
                let error = MsgSimple { kind: &COULD_NOT_WRITE_COMPILED_CODE, message: &err };
                eprintln!("{error}");
                return ExitCode::FAILURE;
            }
        };
    } else {
        let extensions = platform.extensions();
        artifacts = match Artifacts::new(src_path, out_path, &extensions) {
            Ok(new_artifacts) => new_artifacts,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            },
        };

        Logger::info(&COMPILING, src_path, verbosity);
        compilation_sub_step = Logger::new();
    }

    if let Language::Kay | Language::Asm = language {
        let _assembler_status: () = {
            let assembly_src_path = match language {
                // using the freshly generated assembly file
                Language::Kay => &artifacts.asm_path,
                // using the assembly file specified at the command line
                Language::Asm => src_path,
                Language::Obj => unreachable!(),
            };

            let logger = Logger::new();
            let mut assembler_command = platform.assembler(assembly_src_path, &artifacts.obj_path);
            let assembler_result = assembler_command.output();
            logger.sub_step(&ASSEMBLING, Some(&artifacts.obj_path), verbosity);
            match assembler_result {
                Ok(output) => {
                    if !output.status.success() {
                        let error = MsgSimple {
                            kind: &ASSEMBLING_ERROR,
                            message: &String::from_utf8_lossy(&output.stderr),
                        };
                        eprintln!("{error}");
                        return match output.status.code() {
                            Some(code) => ExitCode::from(code as u8),
                            None => ExitCode::FAILURE,
                        };
                    }
                },
                Err(err) => {
                    let error = MsgSimple { kind: &COULD_NOT_RUN_ASSEMBLER, message: &err };
                    eprintln!("{error}");
                    return ExitCode::FAILURE;
                },
            }
        };
    }

    let _linker_status: () = {
        let obj_src_path = match language {
            // using the freshly generated object file
            Language::Kay | Language::Asm => &artifacts.obj_path,
            // using the assembly file specified at the command line
            Language::Obj => src_path,
        };

        let logger = Logger::new();
        let mut linker_command = platform.linker(obj_src_path, &artifacts.exe_path);
        let linker_result = linker_command.output();
        logger.sub_step(&LINKING, Some(&artifacts.exe_path), verbosity);
        match linker_result {
            Ok(output) => {
                if !output.status.success() {
                    let error =
                        MsgSimple { kind: &LINKING_ERROR, message: &String::from_utf8_lossy(&output.stderr) };
                    eprintln!("{error}");
                    return match output.status.code() {
                        Some(code) => ExitCode::from(code as u8),
                        None => ExitCode::FAILURE,
                    };
                }
            },
            Err(err) => {
                let error = MsgSimple { kind: &COULD_NOT_RUN_LINKER, message: &err };
                eprintln!("{error}");
                return ExitCode::FAILURE;
            },
        }
    };

    compilation_sub_step.sub_step(&DONE, None, verbosity);
    execution_step.step(&DONE, None, verbosity);

    return ExitCode::SUCCESS;
}
