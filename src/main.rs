#![allow(clippy::print_stdout, clippy::print_stderr)] // it's a cli tool, it's normal to print to stderr and stdout

use kaylang::{
    artifacts::Artifacts, cli::Args, compiler::Compiler, src_file::SrcFile, syntax::ast::Ast,
    syntax::tokenizer::Tokenizer, Command, Help, Logger, Version, ASSEMBLING, BUILDING_AST, CHECKING,
    COMPILING, GENERATING_ASM, LINKING, LOADING_SOURCE, RUNNING, SUBSTEP_DONE, TOKENIZATION,
};
use std::{env, process::ExitCode};

fn main() -> ExitCode {
    #[allow(unused_mut)]
    let mut env_args = env::args().collect::<Vec<String>>();
    // to quickly debug
    // args.push( "run".to_string() );
    // args.push( "examples/features_test.kay".to_string() );
    // args.push( "-o".to_string() );
    // args.push( "out".to_string() );
    // args.push( "-V".to_string() );

    let Args { color, verbosity, command } = match Args::try_from(env_args) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    };

    color.set(&std::io::stderr());

    if let Command::Version = command {
        println!("{}", Version { color });
        return ExitCode::SUCCESS;
    }

    if let Command::Help { executable_name } = command {
        println!("{}", Help { executable_name, color });
        return ExitCode::SUCCESS;
    }

    let (Command::Check { src_path }
        | Command::Compile { src_path, .. }
        | Command::Run { src_path, .. }) = &command
    else {
        unreachable!()
    };

    let execution_step = Logger::new();

    Logger::info_with_verbosity(&CHECKING, src_path, verbosity);
    let checking_sub_step = Logger::new();

    let src = {
        let loading_source_sub_step = Logger::new();
        let source_loading_result = SrcFile::load(src_path);
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
                let mut errors_iter = errors.into_iter();
                let Some(last_err) = errors_iter.next_back() else {
                    unreachable!("at least one error should be present in this branch");
                };

                for err in errors_iter {
                    eprintln!("{err}\n");
                }
                eprintln!("{last_err}");
                return ExitCode::FAILURE;
            }
        }
    };

    let ast = {
        let building_ast_sub_step = Logger::new();
        let building_ast_result = Ast::build(&src, &tokens);
        building_ast_sub_step.sub_step_done_with_verbosity(&BUILDING_AST, verbosity);
        match building_ast_result {
            Ok(ast) => ast,
            Err(errors) => {
                let mut errors_iter = errors.into_iter();
                let Some(last_err) = errors_iter.next_back() else {
                    unreachable!("at least one error should be present in this branch");
                };

                for err in errors_iter {
                    eprintln!("{err}\n");
                }
                eprintln!("{last_err}");
                return ExitCode::FAILURE;
            }
        }
    };

    checking_sub_step.sub_step_done_with_verbosity(&SUBSTEP_DONE, verbosity);

    let (Command::Compile { out_path, .. } | Command::Run { out_path, .. }) = &command
    else {
        execution_step.step_done_with_verbosity(verbosity);
        return ExitCode::SUCCESS;
    };

    Logger::info_with_verbosity(&COMPILING, src_path, verbosity);
    let compilation_sub_step = Logger::new();

    let artifacts = match Artifacts::new(&src, out_path.as_deref()) {
        Ok(artifacts) => artifacts,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    };

    let _compiler_result: () = {
        let generating_asm_sub_step = Logger::new();
        let compiler_result = Compiler::compile(&src, &artifacts, &ast);
        generating_asm_sub_step.sub_step_done_with_verbosity(&GENERATING_ASM, verbosity);
        match compiler_result {
            Ok(()) => (),
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        }
    };

    let _assembler_status: () = {
        let assembling_sub_step = Logger::new();
        let mut assembler_command = artifacts.assembler();
        let assembler_result = assembler_command.status();
        assembling_sub_step.sub_step_done_with_verbosity(&ASSEMBLING, verbosity);
        match assembler_result {
            Ok(status) => {
                if !status.success() {
                    return ExitCode::from(status.code().unwrap_or(1) as u8);
                }
            }
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        }
    };

    let _linker_status: () = {
        let linking_sub_step = Logger::new();
        let mut linker_command = artifacts.linker();
        let linker_result = linker_command.status();
        linking_sub_step.sub_step_done_with_verbosity(&LINKING, verbosity);
        match linker_result {
            Ok(status) => {
                if !status.success() {
                    return ExitCode::from(status.code().unwrap_or(1) as u8);
                }
            }
            Err(err) => {
                eprintln!("{err}");
                return ExitCode::FAILURE;
            }
        }
    };

    compilation_sub_step.sub_step_done_with_verbosity(&SUBSTEP_DONE, verbosity);

    let Command::Run { .. } = command else {
        execution_step.step_done_with_verbosity(verbosity);
        return ExitCode::SUCCESS;
    };

    Logger::info_with_verbosity(&RUNNING, &artifacts.exe_path, verbosity);

    let mut run_command = artifacts.runner();
    match run_command.status() {
        Ok(status) => {
            if !status.success() {
                return ExitCode::from(status.code().unwrap_or(1) as u8);
            }
        }
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    }

    return ExitCode::SUCCESS;
}

#[cfg(test)]
mod tests {
    use kaylang::{
        artifacts::Artifacts,
        compiler::Compiler,
        src_file::SrcFile,
        syntax::ast::Ast,
        syntax::tokenizer::Tokenizer,
        Color, Logger, Verbosity, ASSEMBLING, BUILDING_AST, CHECKING, COMPILING, GENERATING_ASM,
        LINKING, LOADING_SOURCE, RUNNING, SUBSTEP_DONE, TOKENIZATION,
    };
    use std::{path::{Path, PathBuf}, process::ExitCode};

    #[allow(unused_mut)]
    #[test]
    fn check_examples() -> Result<ExitCode, std::io::Error> {
        let verbosity = Verbosity::Normal;
        let color = Color::Auto;
        let out_path = PathBuf::from("out");

        color.set(&std::io::stderr());
        color.set(&std::io::stdout());

        let src_files = Path::new("examples").read_dir()?;

        for src_file in src_files {
            let src_path = src_file?.path();

            let Some(file_name) = src_path.file_name() else {
                continue;
            };

            if file_name == "features_test.kay" || file_name == "fizzbuzz.kay" {
                continue;
            }

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
                        return Ok(ExitCode::FAILURE);
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
                        let mut errors_iter = errors.into_iter();
                        let Some(last_err) = errors_iter.next_back() else {
                            unreachable!("at least one error should be present in this branch");
                        };

                        for err in errors_iter {
                            eprintln!("{err}\n");
                        }
                        eprintln!("{last_err}");
                        return Ok(ExitCode::FAILURE);
                    }
                }
            };

            let ast = {
                let building_ast_sub_step = Logger::new();
                let building_ast_result = Ast::build(&src, &tokens);
                building_ast_sub_step.sub_step_done_with_verbosity(&BUILDING_AST, verbosity);
                match building_ast_result {
                    Ok(ast) => ast,
                    Err(errors) => {
                        let mut errors_iter = errors.into_iter();
                        let Some(last_err) = errors_iter.next_back() else {
                            unreachable!("at least one error should be present in this branch");
                        };

                        for err in errors_iter {
                            eprintln!("{err}\n");
                        }
                        eprintln!("{last_err}");
                        return Ok(ExitCode::FAILURE);
                    }
                }
            };

            checking_sub_step.sub_step_done_with_verbosity(&SUBSTEP_DONE, verbosity);

            Logger::info_with_verbosity(&COMPILING, &src_path, verbosity);
            let compilation_sub_step = Logger::new();

            let artifacts = match Artifacts::new(&src, Some(&out_path)) {
                Ok(artifacts) => artifacts,
                Err(err) => {
                    eprintln!("{err}");
                    return Ok(ExitCode::FAILURE);
                }
            };

            let _compiler_result: () = {
                let generating_asm_sub_step = Logger::new();
                let compiler_result = Compiler::compile(&src, &artifacts, &ast);
                generating_asm_sub_step.sub_step_done_with_verbosity(&GENERATING_ASM, verbosity);
                match compiler_result {
                    Ok(()) => (),
                    Err(err) => {
                        eprintln!("{err}");
                        return Ok(ExitCode::FAILURE);
                    }
                }
            };

            let _assembler_status: () = {
                let assembling_sub_step = Logger::new();
                let mut assembler_command = artifacts.assembler();
                let assembler_result = assembler_command.status();
                assembling_sub_step.sub_step_done_with_verbosity(&ASSEMBLING, verbosity);
                match assembler_result {
                    Ok(status) => {
                        if !status.success() {
                            return Ok(ExitCode::from(status.code().unwrap_or(1) as u8));
                        }
                    }
                    Err(err) => {
                        eprintln!("{err}");
                        return Ok(ExitCode::FAILURE);
                    }
                }
            };

            let _linker_status: () = {
                let linking_sub_step = Logger::new();
                let mut linker_command = artifacts.linker();
                let linker_result = linker_command.status();
                linking_sub_step.sub_step_done_with_verbosity(&LINKING, verbosity);
                match linker_result {
                    Ok(status) => {
                        if !status.success() {
                            return Ok(ExitCode::from(status.code().unwrap_or(1) as u8));
                        }
                    }
                    Err(err) => {
                        eprintln!("{err}");
                        return Ok(ExitCode::FAILURE);
                    }
                }
            };

            compilation_sub_step.sub_step_done_with_verbosity(&SUBSTEP_DONE, verbosity);
            execution_step.step_done_with_verbosity(verbosity);

            Logger::info_with_verbosity(&RUNNING, &artifacts.exe_path, verbosity);

            let mut run_command = artifacts.runner();
            let run_result = match run_command.output() {
                Ok(output) => output,
                Err(err) => {
                    eprintln!("{err}");
                    return Ok(ExitCode::FAILURE);
                }
            };

            let stdout = String::from_utf8_lossy(&run_result.stdout);
            let stderr = String::from_utf8_lossy(&run_result.stderr);

            eprint!("{stderr}");
            eprintln!("{stdout}");

            if !run_result.status.success() {
                return Ok(ExitCode::from(run_result.status.code().unwrap_or(1) as u8));
            }

            let mut lines = stdout.lines();
            let expected = lines.next().unwrap().strip_prefix("expected:").unwrap().trim_start();
            let actual = lines.next().unwrap().strip_prefix("actual:").unwrap().trim_start();

            if !actual.starts_with("# TODO") {
                assert_eq!(expected, actual);
            }
        }

        return Ok(ExitCode::SUCCESS);
    }
}
