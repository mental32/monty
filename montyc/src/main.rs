use montyc::prelude::{CompilerOptions, GlobalContext};

use structopt::StructOpt;

// let file = opts.input.clone();

//     // let isa = opts.codegen_settings();

//     let v_opts = opts.clone().verify();

//     let mut global_context = GlobalContext::initialize(&v_opts);
//     let _ = global_context.include_module(&file);

//     let show_ir_for =
//     if let Some(name) = opts.show_ir {
//         Some(global_context.span_ref.borrow_mut().push_grouped(0..name.len(), &name, "<eval>".into()))
//     } else {
//         None
//     };

//     let mut cctx = global_context.as_codegen_module();

//     if let Some(name) = show_ir_for {
//         cctx.build_pending_functions();

//         match cctx.get_func_named(name) {
//             Some(st) => eprintln!("{}", st),
//             None => eprintln!("No function found."),
//         }

//         std::process::exit(0);
//     }

//     let object = cctx.finish(None::<&str>);
//     let object = object.to_str().unwrap();

//     let cc = opts.cc.unwrap_or("cc".into());

//     let output = opts.output.unwrap_or_else(|| {
//         file.file_stem()
//             .unwrap()
//             .to_string_lossy()
//             .to_string()
//             .into()
//     });

//     let output = output.to_str().unwrap();

//     std::process::Command::new(cc)
//         .args(&[object, "-o", output, "-no-pie"])
//         .status()
//         .map(|_| ())

fn main() -> std::io::Result<()> {
    env_logger::init();

    let opts = CompilerOptions::from_args();
    let opts2 = opts.clone().verify();

    let mut gcx = GlobalContext::initialize(&opts2);

    let (_, input) = match &opts {
        CompilerOptions::Check { libstd, input }
        | CompilerOptions::Expand { libstd, input }
        | CompilerOptions::Build { libstd, input, .. } => (libstd, input),
    };

    if let Err(err) = gcx.include_module(input, "__main__") {
        panic!("{:?}", err); // TODO: re-implement codespan error handling.
    }

    if let CompilerOptions::Expand { .. } = opts {
        // Note: Might remove this.
        todo!();
    }

    if let CompilerOptions::Build {
        output: _,
        show_ir,
        cc: _,
        ld: _,
        cranelift_settings: _,
        entry,
        ..
    } = opts
    {
        let _functions = gcx.lower_functions_to_ir_starting_from(entry);

        if let Some(_path) = show_ir.as_ref() {
            todo!("show_ir");
        } else {
            todo!("submit lowered functions to cranelift and produce a binary.");
        }
    }

    Ok(())
}
