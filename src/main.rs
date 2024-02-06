use std::{
    env::{self},
    process::ExitCode,
};

use kaylang::Kay;

fn main() -> ExitCode {
    #[allow(unused_mut)]
    let mut args: Vec<String> = env::args().collect();
    // // to quickly debug
    // args.push( "-c".to_string() );
    // args.push( "auto".to_string() );
    // args.push( "run".to_string() );
    // args.push( "examples/features_test.kay".to_string() );
    // args.push( "-o".to_string() );
    // args.push( "examples/out".to_string() );
    // args.push( "-V".to_string() );

    let mut kay = match Kay::try_from(args) {
        Ok(args) => args,
        Err(err) => {
            eprintln!("{}", err);
            return ExitCode::FAILURE;
        }
    };

    return match kay.execute() {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprint!("{}", err);
            ExitCode::FAILURE
        }
    };
}

#[cfg(test)]
mod tests {
    use std::{io, path::Path, process::ExitCode};

    use kaylang::*;

    #[allow(unused_mut)]
    #[test]
    fn check_examples() -> Result<ExitCode, io::Error> {
        let src_files = Path::new("examples").read_dir()?;

        for src_file in src_files {
            let src_path = src_file?.path();

            if let Some(extension) = src_path.extension() {
                if extension == "kay" {
                    // TODO(stefano): run the programs to check for any errors
                    let mut check =
                        Compile { src: src_path.into(), verbosity: Verbosity::Normal, kind: CompileKind::Check };

                    if let Err(err) = check.compile() {
                        eprint!("{}", err);
                        return Ok(ExitCode::FAILURE);
                    }
                }
            }
        }

        return Ok(ExitCode::SUCCESS);
    }
}
