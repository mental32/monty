#![feature(variant_count)]
#![allow(warnings)]

pub mod typing;
pub mod ast;
pub mod parser;
pub mod scope;
pub mod context;
pub mod func;

use structopt::*;

#[derive(Debug, StructOpt)]
pub struct CompilerOptions {
    #[structopt(short, long, parse(from_os_str), default_value = "libstd/")]
    libstd: std::path::PathBuf,

    #[structopt(parse(from_os_str))]
    input: Option<std::path::PathBuf>,
}
