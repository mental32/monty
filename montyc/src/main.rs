use montyc_driver::prelude::{CompilerOptions, SessionContext};

use structopt::StructOpt;

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let opts = CompilerOptions::from_args();
    let opts_verified = match opts.clone().verify() {
        Ok(i) => i,
        Err(mut errors) => {
            for err in errors.drain(..) {
                eprintln!("error: {}", err);
            }

            std::process::exit(1);
        }
    };

    let gcx = match SessionContext::initialize(&opts_verified) {
        Ok(cx) => cx,

        Err((cx, err)) => {
            // TODO: cx.fmt_error(err)
            eprintln!("error: {}", err);
            std::process::exit(1)
        }
    };

    match opts {
        CompilerOptions::Check { input, .. } => {
            gcx.include_module(input, "__main__")?;
        }

        CompilerOptions::Build { .. } => {
            montyc_codegen::compile(&opts, &gcx)?;
        }
    }

    Ok(())
}
