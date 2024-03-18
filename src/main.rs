use kaylang::{
    artifacts::Artifacts, ast::Ast, cli::Args, compiler::Compiler, src_file::SrcFile,
    tokenizer::Tokenizer, Help, Command, Step, SubStep, Version, ASSEMBLING, BUILDING_AST,
    CHECKING, COMPILING, GENERATING_ASM, LINKING, LOADING_SOURCE, RUNNING, SUBSTEP_DONE,
    TOKENIZATION,
};
use std::{env, process::ExitCode, time::Instant};

fn main() -> ExitCode {
    #[allow(unused_mut)]
    let mut args = env::args().collect::<Vec<String>>();
    // to quickly debug
    // args.push( "run".to_string() );
    // args.push( "examples/features_test.kay".to_string() );
    // args.push( "-o".to_string() );
    // args.push( "examples/out".to_string() );
    // args.push( "-V".to_string() );

    let Args { color, verbosity, command } = match Args::try_from(args) {
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
            let execution_step = Step { start_time: Instant::now(), verbosity };

            Step::info(&CHECKING, src_path, verbosity);
            let checking_sub_step =
                SubStep { step: &SUBSTEP_DONE, start_time: Instant::now(), verbosity };

            let loading_source_sub_step =
                SubStep { step: &LOADING_SOURCE, start_time: Instant::now(), verbosity };
            let source_loading_result = SrcFile::load(src_path);
            loading_source_sub_step.done();
            let src = match source_loading_result {
                Ok(src) => src,
                Err(err) => {
                    eprintln!("{err}");
                    return ExitCode::FAILURE;
                }
            };

            let lexing_sub_step =
                SubStep { step: &TOKENIZATION, start_time: Instant::now(), verbosity };
            let lexer_result = Tokenizer::tokenize(&src);
            lexing_sub_step.done();
            let tokens = match lexer_result {
                Ok(tokens) => tokens,
                Err(errors) => {
                    let mut errors_iter = errors.into_iter();
                    let last_err = errors_iter.next_back().unwrap();
                    for err in errors_iter {
                        eprintln!("{err}\n");
                    }
                    eprintln!("{last_err}");
                    return ExitCode::FAILURE;
                }
            };

            let ast_building_sub_step =
                SubStep { step: &BUILDING_AST, start_time: Instant::now(), verbosity };
            let ast_building_result = Ast::build(&src, &tokens);
            ast_building_sub_step.done();
            let ast = match ast_building_result {
                Ok(ast) => ast,
                Err(errors) => {
                    let mut errors_iter = errors.into_iter();
                    let last_err = errors_iter.next_back().unwrap();
                    for err in errors_iter {
                        eprintln!("{err}\n");
                    }
                    eprintln!("{last_err}");
                    return ExitCode::FAILURE;
                }
            };

            checking_sub_step.done();

            let (Command::Compile { out_path, .. } | Command::Run { out_path, .. }) = &command
            else {
                execution_step.done();
                return ExitCode::SUCCESS;
            };

            Step::info(&COMPILING, src_path, verbosity);
            let compilation_sub_step =
                SubStep { step: &SUBSTEP_DONE, start_time: Instant::now(), verbosity };

            let artifacts = match Artifacts::new(&src, out_path.as_ref()) {
                Ok(artifacts) => artifacts,
                Err(err) => {
                    eprintln!("{err}");
                    return ExitCode::FAILURE;
                }
            };

            let asm_generation_sub_step =
                SubStep { step: &GENERATING_ASM, start_time: Instant::now(), verbosity };
            let asm_generation_result = Compiler::compile(&src, &artifacts, &ast);
            asm_generation_sub_step.done();
            match asm_generation_result {
                Ok(()) => {}
                Err(err) => {
                    eprintln!("{err}");
                    return ExitCode::FAILURE;
                }
            };

            let assembler_sub_step =
                SubStep { step: &ASSEMBLING, start_time: Instant::now(), verbosity };
            let mut assembler_command = artifacts.assembler();
            let assembler_result = assembler_command.status();
            assembler_sub_step.done();
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

            let linker_sub_step = SubStep { step: &LINKING, start_time: Instant::now(), verbosity };
            let mut linker_command = artifacts.linker();
            let linker_result = linker_command.status();
            linker_sub_step.done();
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

            compilation_sub_step.done();
            execution_step.done();

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

    ExitCode::SUCCESS
}

#[cfg(test)]
mod tests {
    use kaylang::{
        artifacts::Artifacts,
        ast::Ast,
        cli::{DirPath, FilePath},
        compiler::Compiler,
        src_file::SrcFile,
        tokenizer::Tokenizer,
        Color, Step, SubStep, Verbosity, ASSEMBLING, BUILDING_AST, CHECKING, COMPILING,
        GENERATING_ASM, LINKING, LOADING_SOURCE, RUNNING, SUBSTEP_DONE, TOKENIZATION,
    };
    use std::{io, path::Path, process::ExitCode, time::Instant};

    #[allow(unused_mut)]
    #[test]
    fn check_examples() -> Result<ExitCode, io::Error> {
        let verbosity = Verbosity::Normal;
        let color = Color::Auto;
        let out_path = DirPath::from("examples/out").unwrap();

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

            let execution_step = Step { start_time: Instant::now(), verbosity };

            Step::info(&CHECKING, &src_path, verbosity);
            let checking_sub_step =
                SubStep { step: &SUBSTEP_DONE, start_time: Instant::now(), verbosity };

            let loading_source_sub_step =
                SubStep { step: &LOADING_SOURCE, start_time: Instant::now(), verbosity };
            let source_loading_result = SrcFile::load(&src_path);
            loading_source_sub_step.done();
            let src = match source_loading_result {
                Ok(src) => src,
                Err(err) => {
                    eprintln!("{err}");
                    return Ok(ExitCode::FAILURE);
                }
            };

            let lexing_sub_step =
                SubStep { step: &TOKENIZATION, start_time: Instant::now(), verbosity };
            let lexer_result = Tokenizer::tokenize(&src);
            lexing_sub_step.done();
            let tokens = match lexer_result {
                Ok(tokens) => tokens,
                Err(errors) => {
                    let mut errors_iter = errors.into_iter();
                    let last_err = errors_iter.next_back().unwrap();
                    for err in errors_iter {
                        eprintln!("{err}\n");
                    }
                    eprintln!("{last_err}");
                    return Ok(ExitCode::FAILURE);
                }
            };

            let ast_building_sub_step =
                SubStep { step: &BUILDING_AST, start_time: Instant::now(), verbosity };
            let ast_building_result = Ast::build(&src, &tokens);
            ast_building_sub_step.done();
            let ast = match ast_building_result {
                Ok(ast) => ast,
                Err(errors) => {
                    let mut errors_iter = errors.into_iter();
                    let last_err = errors_iter.next_back().unwrap();
                    for err in errors_iter {
                        eprintln!("{err}\n");
                    }
                    eprintln!("{last_err}");
                    return Ok(ExitCode::FAILURE);
                }
            };

            checking_sub_step.done();

            Step::info(&COMPILING, &src_path, verbosity);
            let compilation_sub_step =
                SubStep { step: &SUBSTEP_DONE, start_time: Instant::now(), verbosity };

            let artifacts = match Artifacts::new(&src, Some(&out_path)) {
                Ok(artifacts) => artifacts,
                Err(err) => {
                    eprintln!("{err}");
                    return Ok(ExitCode::FAILURE);
                }
            };

            let asm_generation_sub_step =
                SubStep { step: &GENERATING_ASM, start_time: Instant::now(), verbosity };
            let asm_generation_result = Compiler::compile(&src, &artifacts, &ast);
            asm_generation_sub_step.done();
            match asm_generation_result {
                Ok(()) => {}
                Err(err) => {
                    eprintln!("{err}");
                    return Ok(ExitCode::FAILURE);
                }
            };

            let assembler_sub_step =
                SubStep { step: &ASSEMBLING, start_time: Instant::now(), verbosity };
            let mut assembler_command = artifacts.assembler();
            let assembler_result = assembler_command.status();
            assembler_sub_step.done();
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

            let linker_sub_step = SubStep { step: &LINKING, start_time: Instant::now(), verbosity };
            let mut linker_command = artifacts.linker();
            let linker_result = linker_command.status();
            linker_sub_step.done();
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

            compilation_sub_step.done();
            execution_step.done();

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

        Ok(ExitCode::SUCCESS)
    }
}
