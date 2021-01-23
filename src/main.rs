use anyhow::Result;
use structopt::StructOpt;

#[derive(Clone, Debug, StructOpt)]
struct Options {
    #[structopt()]
    program: String,
}

fn main() -> Result<()> {
    let options = Options::from_args();

    // parse expression
    //     let program = smawk::parse_program(&options.program)?;
    //
    //     let stdin = std::io::stdin();
    //     let input = stdin.lock();
    //
    //     let stdout = std::io::stdout();
    //     let output = stdout.lock();
    //
    // smawk::execute_program(&program, input, output)?;

    Ok(())
}
