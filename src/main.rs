#![expect(clippy::print_stdout, clippy::print_stderr, reason = "it's a cli tool")]
//! This file is also an example of how it's possible to create cli tools based on this compiler

use kaylang::{
    Args, ArgsParser, Command, Help, Language, Logger, TRANSFORMING, Version, back_end::ir, code_generation::{Artifacts, Platform, nasm }, error::MsgSimple, front_end::{
        src_file::SrcFile, syntax_tree, tokenizer::{TokenizationResult, TokenizedCode, Tokenizer}, typed_abstract_syntax_tree,
    },
};
use kaylang::{
    GENERATING_IR, ASSEMBLING, ASSEMBLING_ERROR, PARSING, COMPILING, COULD_NOT_RUN_ASSEMBLER,
    COULD_NOT_RUN_EXECUTABLE, COULD_NOT_RUN_LINKER, COULD_NOT_WRITE_COMPILED_CODE, DONE, GENERATING_ASM,
    LINKING, LINKING_ERROR, LOADING_SOURCE, PARSING_SYNTAX_TREE, RUNNING,
    TOKENIZATION, TYPE_CHECKING,
};
use std::{path::Path, process::ExitCode};

fn main() -> ExitCode {
    let mut env_args = std::env::args();
    let executable_name_option = env_args.next();
    let executable_name = match &executable_name_option {
        Some(executable_name) => Path::new(executable_name),
        None => Help::default_executable_name(),
    };


    let args = env_args.collect::<Vec<String>>();
    let (color, command) = {
        let Args { color, command } = ArgsParser::parse(&args);

        match command {
            Ok(parsed_command) => (color, parsed_command),
            Err(errors) => {
                color.set(&std::io::stderr());

                let errors_display = kaylang::Errors { executable_name: Some(executable_name), args: &args, errors };
                eprint!("{errors_display}");
                return ExitCode::FAILURE;
            },
        }
    };

    let (src_path, verbosity, language) = match &command {
        Command::Help => {
            color.set(&std::io::stdout());
            let help = Help { executable_name };
            println!("{help}");
            return ExitCode::SUCCESS;
        }
        Command::Version => {
            color.set(&std::io::stdout());
            let version = Version;
            println!("{version}");
            return ExitCode::SUCCESS;
        },

        Command::Check { src_path, verbosity } => (src_path, *verbosity, Language::Kay),

        Command::Compile { src_path, verbosity, language, .. }
        | Command::Run { src_path, verbosity, language, .. } => (src_path, *verbosity, *language),

    };

    color.set(&std::io::stderr());

    let platform = Platform::Auto;
    let artifacts: Artifacts;
    let compilation_sub_step: Logger;

    let execution_step = Logger::new();
    if let Language::Kay = language {
        Logger::info(&PARSING, src_path, verbosity);
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

        let tokens = {
            let logger = Logger::new();
            let TokenizationResult { src, result } = Tokenizer::tokenize(&src_file);
            logger.sub_step(&TOKENIZATION, None, verbosity);
            match result {
                Ok(tokens) => TokenizedCode { src, tokens },
                Err(errors) => {
                    for error in errors {
                        eprintln!("{}\n", error.display(&src));
                    }
                    return ExitCode::FAILURE;
                },
            }
        };

        let st = {
            let logger = Logger::new();
            let result = syntax_tree::Parser::parse(&tokens);
            logger.sub_step(&PARSING_SYNTAX_TREE, None, verbosity);
            match result {
                Ok(st) => st,
                Err(errors) => {
                    for error in errors {
                        eprintln!("{}\n", error.display(&tokens.src));
                    }
                    return ExitCode::FAILURE;
                },
            }
        };

        let tast = {
            let logger = Logger::new();
            let result = typed_abstract_syntax_tree::Parser::parse(&st);
            logger.sub_step(&TYPE_CHECKING, None, verbosity);
            match result {
                Ok(tast) => tast,
                Err(errors) => {
                    for error in errors {
                        eprintln!("{}\n", error.display(&tokens.src));
                    }
                    return ExitCode::FAILURE;
                },
            }
        };

        checking_sub_step.sub_step(&DONE, None, verbosity);

        Logger::info(&TRANSFORMING, src_path, verbosity);
        let transforming_sub_step = Logger::new();

        let ir = {
            let logger = Logger::new();
            let ir = ir::Parser::parse(&tast);
            logger.sub_step(&GENERATING_IR, None, verbosity);
            ir
        };


        transforming_sub_step.sub_step(&DONE, None, verbosity);

        let (Command::Compile { out_path, .. } | Command::Run { out_path, .. }) = &command else {
            execution_step.step(&DONE, None, verbosity);

            eprintln!("{}", ir.display_debug());
            eprintln!("--------------------------------");
            eprintln!("{}", ir.display());

            return ExitCode::SUCCESS;
        };

        Logger::info(&COMPILING, src_path, verbosity);
        compilation_sub_step = Logger::new();

        let extensions = platform.extensions();
        artifacts = match Artifacts::new(src_path, out_path, &extensions) {
            Ok(new_artifacts) => new_artifacts,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            },
        };

        // let _compiler_result_ast: () = {
        //     let logger = Logger::new();
        //     let compiled_code = nasm::compiler_ast::Compiler::compile(&src, &ast);
        //     logger.sub_step(&GENERATING_ASM, Some(&artifacts.asm_path), verbosity);
        //     if let Err(err) = std::fs::write(&artifacts.asm_path, compiled_code) {
        //         let error = MsgSimple { kind: &COULD_NOT_WRITE_COMPILED_CODE, message: &err };
        //         eprintln!("{error}");
        //         return ExitCode::FAILURE;
        //     }
        // };

        let _compiler_result_ir: () = {
            let logger = Logger::new();
            let compiled_code = nasm::compiler_ir::Compiler::compile(&ir);
            logger.sub_step(&GENERATING_ASM, Some(&artifacts.asm_path), verbosity);
            if let Err(err) = std::fs::write(&artifacts.asm_path, compiled_code) {
                let error = MsgSimple { kind: &COULD_NOT_WRITE_COMPILED_CODE, message: &err };
                eprintln!("{error}");
                return ExitCode::FAILURE;
            }
        };
    } else {
        let (Command::Compile { out_path, .. } | Command::Run { out_path, .. }) = &command else { unreachable!() };

        Logger::info(&COMPILING, src_path, verbosity);
        compilation_sub_step = Logger::new();

        let extensions = platform.extensions();
        artifacts = match Artifacts::new(src_path, out_path, &extensions) {
            Ok(new_artifacts) => new_artifacts,
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            },
        };
    }

    if let Language::Kay | Language::Asm = language {
        let _assembler_status: () = {
            let assembly_src_path = match language {
                // using the freshly generated assembly file
                Language::Kay => &artifacts.asm_path,
                // using the assembly file specified at the command line
                Language::Asm => *src_path,
                Language::Obj => unreachable!(),
            };

            let logger = Logger::new();
            let mut assembler_command = platform.assembler(assembly_src_path, &artifacts.obj_path);
            let assembler_result = assembler_command.output();
            logger.sub_step(&ASSEMBLING, Some(&artifacts.obj_path), verbosity);
            match assembler_result {
                Ok(output) => if !output.status.success() {
                    let error = MsgSimple {
                        kind: &ASSEMBLING_ERROR,
                        message: &String::from_utf8_lossy(&output.stderr),
                    };
                    eprintln!("{error}");
                    return match output.status.code() {
                        Some(code) => ExitCode::from(code as u8),
                        None       => ExitCode::FAILURE,
                    };
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
            Language::Obj => *src_path,
        };

        let logger = Logger::new();
        let mut linker_command = platform.linker(obj_src_path, &artifacts.exe_path);
        let linker_result = linker_command.output();
        logger.sub_step(&LINKING, Some(&artifacts.exe_path), verbosity);
        match linker_result {
            Ok(output) => if !output.status.success() {
                let error = MsgSimple { kind: &LINKING_ERROR, message: &String::from_utf8_lossy(&output.stderr) };
                eprintln!("{error}");
                return match output.status.code() {
                    Some(code) => ExitCode::from(code as u8),
                    None => ExitCode::FAILURE,
                };
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

    let Command::Run { .. } = command else { return ExitCode::SUCCESS; };

    let exe_path = Path::new(".").join(&artifacts.exe_path);
    Logger::info(&RUNNING, &exe_path, verbosity);

    let mut run_command = std::process::Command::new(exe_path);
    match run_command.status() {
        Ok(status) => if !status.success() {
            return match status.code() {
                Some(code) => ExitCode::from(code as u8),
                None => ExitCode::FAILURE,
            };
        },
        Err(err) => {
            let error = MsgSimple { kind: &COULD_NOT_RUN_EXECUTABLE, message: &err };
            eprintln!("{error}");
            return ExitCode::FAILURE;
        },
    }

    return ExitCode::SUCCESS;
}
