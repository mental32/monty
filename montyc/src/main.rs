use std::path::PathBuf;

use clap::Parser;
use montyc_driver::{SessionContext, SessionMode, SessionOpts};

use opts::{CompilerOptions, VerifiedCompilerOptions};
use tracing_subscriber::EnvFilter;

mod opts;

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

    let opts = CompilerOptions::parse();
    let VerifiedCompilerOptions(opts) = match opts.verify() {
        Ok(i) => i,
        Err(mut errors) => {
            for err in errors.drain(..) {
                eprintln!("error: {}", err);
            }

            std::process::exit(1);
        }
    };

    let gcx = SessionContext::new_uninit(SessionOpts {
        input: opts.input(),
        libstd: opts.libstd(),
        mode: match opts {
            CompilerOptions::Check { .. } => SessionMode::Check,
            CompilerOptions::Build { .. } => SessionMode::Build,
        },
    })
    .initialize();

    match opts {
        CompilerOptions::Check { input, .. } => {
            gcx.include_module(input, "__main__")?;
        }

        CompilerOptions::Build {
            entry,
            output,
            cc,
            cranelift_settings: settings,
            ..
        } => {
            montyc_codegen::compile(
                montyc_codegen::CgOpts {
                    settings,
                    cc: cc.unwrap_or(PathBuf::from("cc")),
                    output,
                },
                &gcx,
                &entry,
            )?;
        }
    }

    Ok(())
}
