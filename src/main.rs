use kaylang::{
    assembler::Assembler,
    ast::Ast,
    cli::Args,
    compiler::Compiler,
    linker::Linker,
    logging::{
        Step, SubStep, ASM_GENERATION, ASSEMBLER, AST_BUILDING, CHECKING, COMPILING, LEXING, LINKER, LOADING_SOURCE,
        RUNNING, SUBSTEP_DONE,
    },
    run::Run,
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
        RunMode::Check { src_path } | RunMode::Compile { src_path, .. } | RunMode::Run { src_path, .. } => {
            let execution_step = Step { start_time: Instant::now(), verbosity };

            Step::info(&CHECKING, src_path, verbosity);
            let checking_sub_step = SubStep { step: &SUBSTEP_DONE, start_time: Instant::now(), verbosity };

            let src = {
                let loading_source_sub_step = SubStep { step: &LOADING_SOURCE, start_time: Instant::now(), verbosity };
                let source_loading_result = SrcFile::load(src_path);
                loading_source_sub_step.done();
                match source_loading_result {
                    Ok(src) => src,
                    Err(err) => {
                        eprintln!("{err}");
                        return ExitCode::FAILURE;
                    }
                }
            };

            let tokens = {
                let lexing_sub_step = SubStep { step: &LEXING, start_time: Instant::now(), verbosity };
                let lexer_result = Tokenizer::tokenize(&src);
                lexing_sub_step.done();
                match lexer_result {
                    Ok(tokens) => tokens,
                    Err(errors) => {
                        eprintln!();
                        for error in errors {
                            eprintln!("{error}\n");
                        }
                        return ExitCode::FAILURE;
                    }
                }
            };

            let ast = {
                let ast_building_sub_step = SubStep { step: &AST_BUILDING, start_time: Instant::now(), verbosity };
                let ast_building_result = Ast::build(&src, &tokens);
                ast_building_sub_step.done();
                match ast_building_result {
                    Ok(ast) => ast,
                    Err(errors) => {
                        eprintln!();
                        for error in errors {
                            eprintln!("{error}\n");
                        }
                        return ExitCode::FAILURE;
                    }
                }
            };

            checking_sub_step.done();

            if let RunMode::Compile { out_path, .. } | RunMode::Run { out_path, .. } = &run_mode {
                Step::info(&COMPILING, src_path, verbosity);
                let compilation_sub_step = SubStep { step: &SUBSTEP_DONE, start_time: Instant::now(), verbosity };

                let (asm_path, obj_path, exe_path) = {
                    let asm_generation_sub_step =
                        SubStep { step: &ASM_GENERATION, start_time: Instant::now(), verbosity };
                    let asm_generation_result = Compiler::compile(src_path, out_path.as_deref(), &ast);
                    asm_generation_sub_step.done();
                    match asm_generation_result {
                        Ok(artifacts_path) => artifacts_path,
                        Err(err) => {
                            eprintln!("{err}");
                            return ExitCode::FAILURE;
                        }
                    }
                };

                {
                    let assembler_sub_step = SubStep { step: &ASSEMBLER, start_time: Instant::now(), verbosity };
                    let assembler_result = Assembler::assemble(&asm_path, &obj_path);
                    assembler_sub_step.done();
                    match assembler_result {
                        Ok(()) => {}
                        Err(err) => {
                            eprintln!("{err}");
                            return ExitCode::FAILURE;
                        }
                    }
                }

                {
                    let linker_sub_step = SubStep { step: &LINKER, start_time: Instant::now(), verbosity };
                    let linker_result = Linker::link(&obj_path, &exe_path);
                    linker_sub_step.done();
                    match linker_result {
                        Ok(()) => {}
                        Err(err) => {
                            eprintln!("{err}");
                            return ExitCode::FAILURE;
                        }
                    }
                }

                compilation_sub_step.done();
                execution_step.done();

                if let RunMode::Run { .. } = run_mode {
                    Step::info(&RUNNING, &exe_path, verbosity);
                    match Run::run(&exe_path) {
                        Ok(()) => {}
                        Err(err) => {
                            eprintln!("{err}");
                            return ExitCode::FAILURE;
                        }
                    }
                }
            } else {
                execution_step.done();
            }
        }
    }

    ExitCode::SUCCESS
}

#[cfg(test)]
mod tests {
    use kaylang::{
        ast::Ast,
        logging::{Step, SubStep, AST_BUILDING, CHECKING, LEXING, LOADING_SOURCE, SUBSTEP_DONE},
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
        color.set(&std::io::stderr());

        let src_files = Path::new("examples").read_dir()?;

        for src_file in src_files {
            let src_path = src_file?.path();

            if let Some(extension) = src_path.extension() {
                if extension == "kay" {
                    let compilation_step = Step { start_time: Instant::now(), verbosity };

                    Step::info(&CHECKING, &src_path, verbosity);
                    let checking_sub_step = SubStep { step: &SUBSTEP_DONE, start_time: Instant::now(), verbosity };

                    let src = {
                        let loading_source_sub_step =
                            SubStep { step: &LOADING_SOURCE, start_time: Instant::now(), verbosity };
                        let source_loading_result = SrcFile::load(src_path);
                        loading_source_sub_step.done();
                        match source_loading_result {
                            Ok(src) => src,
                            Err(err) => {
                                eprintln!("{err}");
                                return Ok(ExitCode::FAILURE);
                            }
                        }
                    };

                    let tokens = {
                        let lexing_sub_step = SubStep { step: &LEXING, start_time: Instant::now(), verbosity };
                        let lexer_result = Tokenizer::tokenize(&src);
                        lexing_sub_step.done();
                        match lexer_result {
                            Ok(tokens) => tokens,
                            Err(errors) => {
                                eprintln!();
                                for error in errors {
                                    eprintln!("{error}\n");
                                }
                                return Ok(ExitCode::FAILURE);
                            }
                        }
                    };

                    let _ast = {
                        let ast_building_sub_step =
                            SubStep { step: &AST_BUILDING, start_time: Instant::now(), verbosity };
                        let ast_building_result = Ast::build(&src, &tokens);
                        ast_building_sub_step.done();
                        match ast_building_result {
                            Ok(ast) => ast,
                            Err(errors) => {
                                eprintln!();
                                for error in errors {
                                    eprintln!("{error}\n");
                                }
                                return Ok(ExitCode::FAILURE);
                            }
                        }
                    };

                    checking_sub_step.done();
                    compilation_step.done();
                }
            }
        }

        Ok(ExitCode::SUCCESS)
    }
}
