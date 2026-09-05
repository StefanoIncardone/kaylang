#![expect(clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    Color, Logger, Verbosity, code_generation::{ Artifacts, Platform }, error::MsgSimple,
};
use kaylang::{
    ASSEMBLING, ASSEMBLING_ERROR, COMPILING, COULD_NOT_RUN_ASSEMBLER, COULD_NOT_RUN_LINKER, DONE, LINKING, LINKING_ERROR, SUBSTEP_DONE,
};
use std::{path::Path, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    let color = Color::Auto;
    color.set(&std::io::stderr());

    // controls how much information about compilation steps are reported to the user
    let verbosity = Verbosity::Verbose;

    // control the platform to target for code generation
    let platform = Platform::Auto;

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = Path::new("examples/fizzbuzz.asm");
    let out_path = Path::new("out");

    let execution_step = Logger::new();

    let extensions = platform.extensions();
    let artifacts = match Artifacts::new(src_path, out_path, &extensions) {
        Ok(new_artifacts) => new_artifacts,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        },
    };

    Logger::info(&COMPILING, src_path, verbosity);
    let compilation_sub_step = Logger::new();

    let _assembler_status: () = {
        let assembly_src_path = src_path;

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

    let _linker_status: () = {
        // using the freshly generated object file
        let linker_src_path = &artifacts.obj_path;

        let logger = Logger::new();
        let mut linker_command = platform.linker(linker_src_path, &artifacts.exe_path);
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
