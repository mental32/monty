use montyc_driver::prelude::{CompilerOptions, SessionContext};

use structopt::StructOpt;
use tracing_subscriber::EnvFilter;

fn install_logger() {
    let display_filename = std::env::var("MONTYC_DEBUG_FILE").map_or(true, |st| st != "0");

    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_env("RUST_LOG"))
        .with_file(display_filename)
        .with_line_number(display_filename)
        .with_target(true)
        .with_thread_names(true)
        .with_timer(tracing_subscriber::fmt::time::uptime())
        .init();
}

fn main() -> anyhow::Result<()> {
    install_logger();

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

        Err((_cx, err)) => {
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
