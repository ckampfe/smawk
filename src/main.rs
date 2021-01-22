use anyhow::Result;
use structopt::StructOpt;

#[derive(Clone, Debug, StructOpt)]
struct Options {
    #[structopt()]
    program: String,
}

fn main() -> Result<()> {
    let options = Options::from_args();

    let program = "begin {} pattern {} literal {} end {}";
    // parse expression
    let program = smawk::parse_program(&options.program)?;
    smawk::execute_program(&program)?;
    // compile expression
    // execute pre statements
    // execute main loop
    // execute post expressions

    println!("Hello, world!");

    Ok(())
}
