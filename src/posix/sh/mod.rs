use super::{UtilSetup, ArgsIter, Result};

use self::parser::Parser;

mod ast;
mod env;
mod parser;

pub const NAME: &str = "sh";
pub const DESCRIPTION: &str = "Minimal POSIX shell";




pub fn execute<S, T>(setup: &mut S, mut args: T) -> Result<()>
where
    S: UtilSetup,
    T: ArgsIter,
{
    use std::io::Read;

    let mut input = ::std::fs::File::open("input.sh").unwrap();
    let mut data = Vec::new();
    input.read_to_end(&mut data).unwrap();

    let res = Parser::new().complete_command(data.as_slice()).1;
    match res {
        Ok(m) => {
            let mut env = setup.env().into();

            //println!("{:#?}", m);
            //println!();
            println!("status: {}", m.1.execute(setup, &mut env));
        }
        Err(f) => println!("{}", f)
    }

    Ok(())
}
