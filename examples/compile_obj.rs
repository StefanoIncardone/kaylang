#![expect(clippy::print_stderr, reason = "it's a cli tool")]

use kaylang::{
    back_end::artifacts::Artifacts,
    error,
    Color, Logger, COMPILING,
    COULD_NOT_RUN_LINKER, DONE, LINKING,
    LINKING_ERROR, SUBSTEP_DONE,
};
use std::{path::Path, process::ExitCode};

fn main() -> ExitCode {
    // controls how error messages should be colored
    Color::Auto.set(&std::io::stderr());

    // cargo sets the working directory to where the `cargo` command was run.
    // so we assume this example is run from the root of the crate
    let src_path = Path::new("examples/fizzbuzz.o");
    let out_path = Path::new("examples/out");

    let execution_step = Logger::new();

    let artifacts = match Artifacts::new_with_out_path(src_path, out_path) {
        Ok(artifacts) => artifacts,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    };

    Logger::info(&COMPILING, src_path);
    let compilation_sub_step = Logger::new();

    let _linker_status: () = {
        let linking_sub_step = Logger::new();
        let mut linker_command = artifacts.linker();
        let linker_result = linker_command.output();
        linking_sub_step.sub_step(&LINKING, Some(&artifacts.exe_path));
        match linker_result {
            Ok(output) => {
                if !output.status.success() {
                    let error = error::Msg {
                        kind: &LINKING_ERROR,
                        message: &String::from_utf8_lossy(&output.stderr),
                    };
                    eprintln!("{error}");
                    return match output.status.code() {
                        #[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                        Some(code) => ExitCode::from(code as u8),
                        None => ExitCode::FAILURE,
                    };
                }
            }
            Err(err) => {
                let error = error::Msg { kind: &COULD_NOT_RUN_LINKER, message: &err };
                eprintln!("{error}");
                return ExitCode::FAILURE;
            }
        }
    };

    compilation_sub_step.sub_step(&SUBSTEP_DONE, None);
    execution_step.step(&DONE, None);

    return ExitCode::SUCCESS;
}
