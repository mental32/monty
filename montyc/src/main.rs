use std::io;

use montyc::{context::GlobalContext, CompilerOptions};

use structopt::StructOpt;

fn main() -> io::Result<()> {
    env_logger::init();

    let opts = CompilerOptions::from_args();

    let global_context = GlobalContext::from(opts);

    Ok(())
}
