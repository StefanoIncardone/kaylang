#![allow( clippy::needless_return )]
use std::{env::{self}, process::ExitCode};

use kaylang::Kay;


fn main() -> ExitCode {
    #[allow( unused_mut )]
    let mut args: Vec<String> = env::args().collect();
    // // to quickly debug
    // args.push( "-c".to_string() );
    // args.push( "auto".to_string() );
    // args.push( "run".to_string() );
    // args.push( "examples/features_test.kay".to_string() );
    // args.push( "-o".to_string() );
    // args.push( "examples/out".to_string() );
    // args.push( "-V".to_string() );

    let kay = match Kay::try_from( args ) {
        Ok( kay ) => kay,
        Err( err ) => {
            eprintln!( "{}", err );
            return ExitCode::FAILURE;
        },
    };

    match kay.execute() {
        Ok( () ) => ExitCode::SUCCESS,
        Err( err ) => {
            eprintln!( "{}", err );
            ExitCode::FAILURE
        },
    }
}

#[cfg( test )]
mod tests {
    use std::{path::Path, process::ExitCode};

    use kaylang::*;


    #[allow( unused_mut )]
    #[test]
    fn test_checking() -> ExitCode {
        for src_file in Path::new( "./examples" ).read_dir().unwrap() {
            let src_path = src_file.unwrap().path();
            if let Some( extension ) = src_path.extension() {
                if extension == "kay" {
                    let kay: Kay = KayArgs {
                        run_mode: Some( RunMode::Check { src_path } ),
                        ..Default::default()
                    }.into();

                    match kay.execute() {
                        Ok( () ) => {},
                        Err( err ) => {
                            eprintln!( "{}", err );
                            return ExitCode::FAILURE;
                        },
                    }
                }
            }
        }

        return ExitCode::SUCCESS;
    }
}
