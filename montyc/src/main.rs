use std::{io, path::Path};

use argh::FromArgs;

/// Invoke the montyc machinery.
#[derive(Debug, FromArgs)]
struct Args {
    #[argh(positional)]
    file: String,
}

fn main() -> io::Result<()> {
    let args: Args = argh::from_env();

    let path: &Path = args.file.as_ref();

    let contents = std::fs::read_to_string(path)?;

    let parser = montyc::parser::PyParse::new(contents);

    eprintln!("{:#?}", parser.parse_module());

    Ok(())
}
