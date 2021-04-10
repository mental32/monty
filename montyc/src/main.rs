use std::{io, path::{Path, PathBuf}};

use montyc::{
    ast::stmt::Statement, context::GlobalContext, scope::downcast_ref, typing::TypedObject,
};

use structopt::*;

#[derive(Debug, StructOpt)]
pub struct Opts {
    #[structopt(short, long, parse(from_os_str), default_value = "libstd/")]
    libstd: PathBuf,

    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

fn main() -> io::Result<()> {
    let opts = Opts::from_args();

    let std = opts.libstd.canonicalize().unwrap();

    assert!(std.exists());

    let mut ctx = GlobalContext::default();

    // let builtins = {
    //     let builtins_path = std.join("builtins.py");
    //     let builtins = std::fs::read_to_string(&builtins_path)?;
    
    //     ctx.parse_and_register_module(builtins, builtins_path)
    // };

    let main = {
        let path = opts.input.canonicalize().unwrap();
        let source = std::fs::read_to_string(&path)?;

        ctx.parse_and_register_module(source, path)
    };

    dbg!(&ctx.modules.get(&main).unwrap().module);

    // for (object, local_context) in ctx.walk(module_ref) {
    //     let object = match downcast_ref::<Statement>(object.as_ref()) {
    //         Some(inner) => inner,
    //         None => unreachable!(),
    //     };

    //     object.typecheck(local_context);
    // }

    Ok(())
}
