#![allow(clippy::print_stdout, clippy::print_stderr)] // it's a cli tool, it's normal to print to stderr and stdout

use kaylang::{
    artifacts::Artifacts, cli::Args, compiler::Compiler, src_file::SrcFile, syntax::ast::Ast,
    syntax::tokenizer::Tokenizer, Command, Help, Step, Version, ASSEMBLING, BUILDING_AST, CHECKING,
    COMPILING, GENERATING_ASM, LINKING, LOADING_SOURCE, RUNNING, SUBSTEP_DONE, TOKENIZATION,
};
use std::{env, process::ExitCode, time::Instant};

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

    match &command {
        Command::Version => {
            println!("{}", Version { color });
            return ExitCode::SUCCESS;
        }
        Command::Help => {
            println!("{}", Help { color });
            return ExitCode::SUCCESS;
        }
        Command::Check { src_path }
        | Command::Compile { src_path, .. }
        | Command::Run { src_path, .. } => {
            let execution_step = Instant::now();

            Step::info(&CHECKING, src_path, verbosity);
            let checking_sub_step = Instant::now();

            let loading_source_sub_step = Instant::now();
            let source_loading_result = SrcFile::load(src_path);
            Step::sub_step_done(loading_source_sub_step, &LOADING_SOURCE, verbosity);
            let src = match source_loading_result {
                Ok(src) => src,
                Err(err) => {
                    eprintln!("{err}");
                    return ExitCode::FAILURE;
                }
            };

            let tokenization_sub_step = Instant::now();
            let tokenizer_result = Tokenizer::tokenize(&src);
            Step::sub_step_done(tokenization_sub_step, &TOKENIZATION, verbosity);
            let tokens = match tokenizer_result {
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
            };

            let building_ast_sub_step = Instant::now();
            let building_ast_result = Ast::build(&src, &tokens);
            Step::sub_step_done(building_ast_sub_step, &BUILDING_AST, verbosity);
            let ast = match building_ast_result {
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
            };

            Step::sub_step_done(checking_sub_step, &SUBSTEP_DONE, verbosity);

            let (Command::Compile { out_path, .. } | Command::Run { out_path, .. }) = &command
            else {
                Step::step_done(execution_step, verbosity);
                return ExitCode::SUCCESS;
            };

            Step::info(&COMPILING, src_path, verbosity);
            let compilation_sub_step = Instant::now();

            let artifacts = match Artifacts::new(&src, out_path.as_ref()) {
                Ok(artifacts) => artifacts,
                Err(err) => {
                    eprintln!("{err}");
                    return ExitCode::FAILURE;
                }
            };

            let generating_asm_sub_step = Instant::now();
            let compiler_result = Compiler::compile(&src, &artifacts, &ast);
            Step::sub_step_done(generating_asm_sub_step, &GENERATING_ASM, verbosity);
            match compiler_result {
                Ok(()) => {}
                Err(err) => {
                    eprintln!("{err}");
                    return ExitCode::FAILURE;
                }
            };

            let assembling_sub_step = Instant::now();
            let mut assembler_command = artifacts.assembler();
            let assembler_result = assembler_command.status();
            Step::sub_step_done(assembling_sub_step, &ASSEMBLING, verbosity);
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

            let linking_sub_step = Instant::now();
            let mut linker_command = artifacts.linker();
            let linker_result = linker_command.status();
            Step::sub_step_done(linking_sub_step, &LINKING, verbosity);
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

            Step::sub_step_done(compilation_sub_step, &SUBSTEP_DONE, verbosity);
            Step::step_done(execution_step, verbosity);

            if let Command::Run { .. } = command {
                Step::info(&RUNNING, &artifacts.exe_path, verbosity);

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
            }
        }
    }

    return ExitCode::SUCCESS;
}

#[cfg(test)]
mod tests {
    use kaylang::{
        artifacts::Artifacts,
        cli::{DirPath, FilePath},
        compiler::Compiler,
        src_file::SrcFile,
        syntax::ast::Ast,
        syntax::tokenizer::Tokenizer,
        Color, Step, Verbosity, ASSEMBLING, BUILDING_AST, CHECKING, COMPILING, GENERATING_ASM,
        LINKING, LOADING_SOURCE, RUNNING, SUBSTEP_DONE, TOKENIZATION,
    };
    use std::{io, path::Path, process::ExitCode, time::Instant};

    #[allow(unused_mut)]
    #[test]
    fn check_examples() -> Result<ExitCode, io::Error> {
        let verbosity = Verbosity::Normal;
        let color = Color::Auto;
        let Some(out_path) = DirPath::from("out") else {
            unreachable!("the literal dir path should not be a file path");
        };

        color.set(&std::io::stderr());
        color.set(&std::io::stdout());

        let src_files = Path::new("examples").read_dir()?;

        for src_file in src_files {
            let Some(src_path) = FilePath::from(src_file?.path()) else {
                continue;
            };

            let Some(file_name) = src_path.file_name() else {
                continue;
            };

            if file_name == "features_test.kay" || file_name == "fizzbuzz.kay" {
                continue;
            }

            let execution_step = Instant::now();

            Step::info(&CHECKING, &src_path, verbosity);
            let checking_sub_step = Instant::now();

            let loading_source_sub_step = Instant::now();
            let source_loading_result = SrcFile::load(&src_path);
            Step::sub_step_done(loading_source_sub_step, &LOADING_SOURCE, verbosity);
            let src = match source_loading_result {
                Ok(src) => src,
                Err(err) => {
                    eprintln!("{err}");
                    return Ok(ExitCode::FAILURE);
                }
            };

            let tokenization_sub_step = Instant::now();
            let tokenizer_result = Tokenizer::tokenize(&src);
            Step::sub_step_done(tokenization_sub_step, &TOKENIZATION, verbosity);
            let tokens = match tokenizer_result {
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
            };

            let building_ast_sub_step = Instant::now();
            let building_ast_result = Ast::build(&src, &tokens);
            Step::sub_step_done(building_ast_sub_step, &BUILDING_AST, verbosity);
            let ast = match building_ast_result {
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
            };

            Step::sub_step_done(checking_sub_step, &SUBSTEP_DONE, verbosity);

            Step::info(&COMPILING, &src_path, verbosity);
            let compilation_sub_step = Instant::now();

            let artifacts = match Artifacts::new(&src, Some(&out_path)) {
                Ok(artifacts) => artifacts,
                Err(err) => {
                    eprintln!("{err}");
                    return Ok(ExitCode::FAILURE);
                }
            };

            let generating_asm_sub_step = Instant::now();
            let compiler_result = Compiler::compile(&src, &artifacts, &ast);
            Step::sub_step_done(generating_asm_sub_step, &GENERATING_ASM, verbosity);
            match compiler_result {
                Ok(()) => {}
                Err(err) => {
                    eprintln!("{err}");
                    return Ok(ExitCode::FAILURE);
                }
            };

            let assembler_sub_step = Instant::now();
            let mut assembler_command = artifacts.assembler();
            let assembler_result = assembler_command.status();
            Step::sub_step_done(assembler_sub_step, &ASSEMBLING, verbosity);
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

            let linking_sub_step = Instant::now();
            let mut linker_command = artifacts.linker();
            let linker_result = linker_command.status();
            Step::sub_step_done(linking_sub_step, &LINKING, verbosity);
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

            Step::sub_step_done(compilation_sub_step, &SUBSTEP_DONE, verbosity);
            Step::step_done(execution_step, verbosity);

            Step::info(&RUNNING, &artifacts.exe_path, verbosity);

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
