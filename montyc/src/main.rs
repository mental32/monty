use montyc::prelude::{CompilerOptions, GlobalContext};

use structopt::StructOpt;

fn main() -> std::io::Result<()> {
    env_logger::init();

    let opts = CompilerOptions::from_args();
    let opts2 = opts.clone().verify();

    let mut gcx = GlobalContext::initialize(&opts2);

    let (_, input) = match &opts {
        CompilerOptions::Check { libstd, input } | CompilerOptions::Build { libstd, input, .. } => {
            (libstd, input)
        }
    };

    if let Err(err) = gcx.include_module(input, "__main__") {
        panic!("{:?}", err); // TODO: re-implement codespan error handling.
    }

    if let CompilerOptions::Build {
        output,
        show_ir,
        cc,
        ld: _,
        cranelift_settings: _,
        entry,
        ..
    } = opts
    {
        let _functions = gcx.lower_code_starting_from(entry).unwrap();

        if let Some(_path) = show_ir.as_ref() {
            todo!("show_ir");
        }

        let object = todo!();

        let cc = cc.unwrap_or("cc".into());

        let output = output.unwrap_or_else(|| entry.split(":").last().unwrap().into());

        let output = output.to_str().unwrap();

        std::process::Command::new(cc)
            .args(&[object, "-o", output, "-no-pie"])
            .status()
            .map(|_| ())?;
    }

    Ok(())
}
