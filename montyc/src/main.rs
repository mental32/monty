use std::path::PathBuf;

use clap::{Parser, Subcommand};
use miette::{Context, IntoDiagnostic};

use montyc_driver::{CompilerOpts, CompilerQueries};

mod envvar {
    //! any environment variables used by montyc will be declared as constants here and documented.

    #[derive(Debug)]
    pub struct EVars {
        pub debug: Option<String>,
    }

    /// enable extra logging information e.g. file names and line numbers for debugging purposes.
    ///
    pub const DEBUG: &str = "MONTYC_DEBUG";

    /// try and load as many environment variables as possible.
    ///
    pub fn load() -> EVars {
        EVars {
            debug: std::env::var(DEBUG).map(|st| st.to_lowercase()).ok(),
        }
    }
}

mod logging {
    use tracing_subscriber::prelude::*;

    pub fn setup_global_logging(env: &super::envvar::EVars) {
        let mut fmt_layer = tracing_subscriber::fmt::layer();

        if matches!(
            env.debug.as_ref().map(|s| s.as_str()),
            Some("t" | "y" | "1" | "on" | "yes" | "true")
        ) {
            fmt_layer = fmt_layer.with_file(true).with_line_number(true);
        }

        let reg = tracing_subscriber::registry()
            .with(fmt_layer)
            .with(tracing_subscriber::EnvFilter::from_default_env())
            .init();
    }
}

#[derive(Debug, Clone, Subcommand)]
enum OptChoice {
    /// check and build the code into a binary
    ///
    Build {
        /// the path to a file, directory, or pyproject.toml file to use as the build source.
        path: PathBuf,
    },

    /// check a local package and all of its dependencies for errors
    ///
    Check {
        /// the path to a file, directory, or pyproject.toml file to use as the build source.
        path: PathBuf,
    },
}

#[derive(Debug, Parser)]
struct Opts {
    #[command(subcommand)]
    inner: Option<OptChoice>,
    #[arg(long)]
    version: bool,
}

async fn async_main(evars: envvar::EVars, opts: Opts) -> miette::Result<()> {
    let action = match opts.inner {
        Some(action) => action,
        None => miette::bail!("no action was specified"),
    };

    let c_opts = CompilerOpts::new();
    let mut session_builder = c_opts.session_builder();

    match action {
        OptChoice::Build { path } | OptChoice::Check { path } => {
            let metadata = tokio::fs::symlink_metadata(&path)
                .await
                .into_diagnostic()
                .wrap_err("unsable to access file metadata")?;

            if !(metadata.is_file() || metadata.is_dir()) {
                miette::bail!("can only accept paths to files or directories");
            }

            let path = tokio::fs::canonicalize(&path).await.into_diagnostic()?;
            let file_contents = tokio::fs::read_to_string(&path).await.into_diagnostic()?;

            let file_id = session_builder.add_file_to_vfs(path.clone(), file_contents);

            let sess = session_builder.build();

            assert_eq!(sess.file_id_of_path(&path), Some(file_id));

            tracing::info!(tokens = ?sess.raw_tokens_of_file(file_id), "tokens");
            tracing::info!(module = ?sess.module_ast_of_file(file_id), "ast");

            Ok(())
        }
    }
}

fn main() -> miette::Result<()> {
    let evars = envvar::load();
    let _guard = logging::setup_global_logging(&evars);

    tracing::info!(?evars, "collected environment variables");

    let opts = Opts::parse();

    tracing::info!(?opts, "parsed command line options");

    if opts.inner.is_some() {
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .into_diagnostic()?
            .block_on(async_main(evars, opts))
    } else {
        if opts.version {
            println!(
                "{bin} {ver:?}",
                bin = env!("CARGO_PKG_NAME"),
                ver = env!("CARGO_PKG_VERSION"),
            );
        }

        Ok(())
    }
}
