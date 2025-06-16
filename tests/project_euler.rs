#![cfg(test)]

mod common;

use common::run;
use kaylang::Color;
use std::{path::PathBuf, process::ExitCode};

use crate::common::check;

#[expect(clippy::panic, clippy::panic_in_result_fn, reason = "it's for testing")]
#[test]
fn check_project_euler() -> Result<(), ExitCode> {
    let color = Color::Auto;
    color.set(&std::io::stderr());
    color.set(&std::io::stdout());

    let src_files = match std::fs::read_dir("examples/project_euler") {
        Ok(files) => files,
        Err(err) => panic!("could not read project_euler folder: {err}"),
    };

    for src_file in src_files {
        let src_path = match src_file {
            Ok(path) => path.path(),
            Err(err) => panic!("could not get path: {err}"),
        };

        let Some(file_name) = src_path.file_name() else {
            continue;
        };

        if file_name == "features_test.kay" || file_name == "fizzbuzz.kay" {
            continue;
        }

        check(&src_path)?;
    }

    return Ok(());
}

#[expect(clippy::panic, clippy::panic_in_result_fn, reason = "it's for testing")]
#[test]
fn run_project_euler() -> Result<(), ExitCode> {
    let color = Color::Auto;
    color.set(&std::io::stderr());
    color.set(&std::io::stdout());

    let out_path = PathBuf::from("out");
    let src_files = match std::fs::read_dir("examples/project_euler") {
        Ok(files) => files,
        Err(err) => panic!("could not read project_euler folder: {err}"),
    };

    for src_file in src_files {
        let src_path = match src_file {
            Ok(path) => path.path(),
            Err(err) => panic!("could not get path: {err}"),
        };

        let Some(file_name) = src_path.file_name() else {
            continue;
        };

        if file_name == "features_test.kay" || file_name == "fizzbuzz.kay" {
            continue;
        }

        run(&src_path, &out_path)?;
    }

    return Ok(());
}
