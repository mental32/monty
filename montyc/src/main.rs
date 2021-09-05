use montyc::prelude::{CompilerOptions, GlobalContext};

use structopt::StructOpt;

fn main() -> std::io::Result<()> {
    env_logger::init();

    let opts = CompilerOptions::from_args();
    let opts_cg = opts.codegen_settings();
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
        let (mut funcs, entry_ix) = gcx.collect_function_dependencies(&entry).unwrap();

        if let Some(_path) = show_ir.as_ref() {
            todo!("show_ir");
        }

        let object = {
            let mut cg = montyc_codegen::prelude::CodegenModule::new();

            for func in funcs.drain(..) {
                cg.include_function(&mut gcx, func);
            }

            cg.finish(&mut gcx, None::<&str>, opts_cg, entry_ix)?
        };

        let cc = cc.unwrap_or("cc".into());

        let object_path = &*object.to_string_lossy();

        let output = output.to_str().unwrap();

        std::process::Command::new(cc)
            .args(&[object_path, "-o", output, "-no-pie"])
            .status()
            .map(|_| ())?;
    }

    Ok(())
}
