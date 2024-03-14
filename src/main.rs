use kaylang::{
    artifacts::{Artifacts, Assembler, Linker, Run},
    ast::Ast,
    cli::Args,
    compiler::Compiler,
    logging::{
        Step, SubStep, ASM_GENERATION, ASSEMBLER, AST_BUILDING, CHECKING, COMPILING, LINKER,
        LOADING_SOURCE, RUNNING, SUBSTEP_DONE, TOKENIZATION,
    },
    src_file::SrcFile,
    tokenizer::Tokenizer,
    Help, RunMode, Version,
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

    let Args { color, verbosity, run_mode } = match Args::try_from(args) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    };

    color.set(&std::io::stderr());

    match &run_mode {
        RunMode::Version => {
            println!("{}", Version { color });
            return ExitCode::SUCCESS;
        }
        RunMode::Help => {
            println!("{}", Help { color });
            return ExitCode::SUCCESS;
        }
        RunMode::Check { src_path }
        | RunMode::Compile { src_path, .. }
        | RunMode::Run { src_path, .. } => {
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
                    eprintln!();
                    for error in errors {
                        eprintln!("{error}\n");
                    }
                    return ExitCode::FAILURE;
                }
            };

            let ast_building_sub_step =
                SubStep { step: &AST_BUILDING, start_time: Instant::now(), verbosity };
            let ast_building_result = Ast::build(&src, &tokens);
            ast_building_sub_step.done();
            let ast = match ast_building_result {
                Ok(ast) => ast,
                Err(errors) => {
                    eprintln!();
                    for error in errors {
                        eprintln!("{error}\n");
                    }
                    return ExitCode::FAILURE;
                }
            };

            checking_sub_step.done();

            let (RunMode::Compile { out_path, .. } | RunMode::Run { out_path, .. }) = &run_mode
            else {
                execution_step.done();
                return ExitCode::SUCCESS;
            };

            Step::info(&COMPILING, src_path, verbosity);
            let compilation_sub_step =
                SubStep { step: &SUBSTEP_DONE, start_time: Instant::now(), verbosity };

            let Artifacts { asm_path, obj_path, exe_path } =
                match Artifacts::try_from_src(&src, out_path.as_ref()) {
                    Ok(artifacts) => artifacts,
                    Err(err) => {
                        eprintln!("{err}");
                        return ExitCode::FAILURE;
                    }
                };

            let asm_generation_sub_step =
                SubStep { step: &ASM_GENERATION, start_time: Instant::now(), verbosity };
            let asm_generation_result = Compiler::compile(&src, &asm_path, &ast);
            asm_generation_sub_step.done();
            match asm_generation_result {
                Ok(()) => {}
                Err(err) => {
                    eprintln!("{err}");
                    return ExitCode::FAILURE;
                }
            };

            let assembler_sub_step =
                SubStep { step: &ASSEMBLER, start_time: Instant::now(), verbosity };
            let mut assembler_command = Assembler::assemble(&asm_path, &obj_path);
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

            let linker_sub_step = SubStep { step: &LINKER, start_time: Instant::now(), verbosity };
            let mut linker_command = Linker::link(&obj_path, &exe_path);
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

            if let RunMode::Run { .. } = run_mode {
                Step::info(&RUNNING, exe_path.inner(), verbosity);

                let mut run_command = Run::run(&exe_path);
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
        artifacts::{Artifacts, Assembler, Linker, Run},
        ast::Ast,
        cli::Utf8Path,
        compiler::Compiler,
        logging::{
            Step, SubStep, ASM_GENERATION, ASSEMBLER, AST_BUILDING, CHECKING, COMPILING, LINKER,
            LOADING_SOURCE, RUNNING, SUBSTEP_DONE, TOKENIZATION,
        },
        src_file::SrcFile,
        tokenizer::Tokenizer,
        Color, Verbosity,
    };
    use std::{io, path::Path, process::ExitCode, time::Instant};

    #[allow(unused_mut)]
    #[test]
    fn check_examples() -> Result<ExitCode, io::Error> {
        let verbosity = Verbosity::Normal;
        let color = Color::Auto;
        let out_path = Utf8Path::from("examples/out").unwrap();

        color.set(&std::io::stderr());
        color.set(&std::io::stdout());

        let src_files = Path::new("examples").read_dir()?;

        for src_file in src_files {
            let src_path = Utf8Path::from(src_file?.path()).unwrap();
            if src_path.is_dir() {
                continue;
            }

            let Some(extension) = src_path.extension() else {
                continue;
            };

            if extension != "kay" {
                continue;
            }

            match src_path.file_name() {
                Some(path) => {
                    if path == "features_test.kay" || path == "fizzbuzz.kay" {
                        continue;
                    }
                }
                None => continue,
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
                    eprintln!();
                    for error in errors {
                        eprintln!("{error}\n");
                    }
                    return Ok(ExitCode::FAILURE);
                }
            };

            let ast_building_sub_step =
                SubStep { step: &AST_BUILDING, start_time: Instant::now(), verbosity };
            let ast_building_result = Ast::build(&src, &tokens);
            ast_building_sub_step.done();
            let ast = match ast_building_result {
                Ok(ast) => ast,
                Err(errors) => {
                    eprintln!();
                    for error in errors {
                        eprintln!("{error}\n");
                    }
                    return Ok(ExitCode::FAILURE);
                }
            };

            checking_sub_step.done();

            Step::info(&COMPILING, &src_path, verbosity);
            let compilation_sub_step =
                SubStep { step: &SUBSTEP_DONE, start_time: Instant::now(), verbosity };

            let Artifacts { asm_path, obj_path, exe_path } =
                match Artifacts::try_from_src(&src, Some(&out_path)) {
                    Ok(artifacts) => artifacts,
                    Err(err) => {
                        eprintln!("{err}");
                        return Ok(ExitCode::FAILURE);
                    }
                };

            let asm_generation_sub_step =
                SubStep { step: &ASM_GENERATION, start_time: Instant::now(), verbosity };
            let asm_generation_result = Compiler::compile(&src, &asm_path, &ast);
            asm_generation_sub_step.done();
            match asm_generation_result {
                Ok(()) => {}
                Err(err) => {
                    eprintln!("{err}");
                    return Ok(ExitCode::FAILURE);
                }
            };

            let assembler_sub_step =
                SubStep { step: &ASSEMBLER, start_time: Instant::now(), verbosity };
            let mut assembler_command = Assembler::assemble(&asm_path, &obj_path);
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

            let linker_sub_step = SubStep { step: &LINKER, start_time: Instant::now(), verbosity };
            let mut linker_command = Linker::link(&obj_path, &exe_path);
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

            Step::info(&RUNNING, exe_path.inner(), verbosity);

            let mut run_command = Run::run(&exe_path);
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
