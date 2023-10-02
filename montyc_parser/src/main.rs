use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Parser)]
struct Args {
    #[clap(long)]
    emit_json: bool,
    files: Vec<PathBuf>,
}

fn main() {
    let args = Args::parse();

    let mut span_interner = montyc_parser::span_interner::SpanInterner::new();

    let files = args.files.into_iter().map(|st| {
        let file_path = PathBuf::from(st);
        if !file_path.exists() {
            eprintln!("{}: no such file", file_path.display());
            std::process::exit(1);
        } else {
            file_path
        }
    });

    for path in files {
        let file_name = path.file_name().unwrap().to_str().unwrap();
        println!("{}", path.canonicalize().unwrap().display());

        let contents = std::fs::read_to_string(&path).unwrap();

        let (module, errors) = montyc_parser::parse(&mut span_interner, (), &contents);

        if errors.len() > 0 {
            eprintln!("{}: {} errors", file_name, errors.len());
            for err in errors {
                eprintln!("  {err:?}");
            }
        } else {
            let Some(module) = module else { unreachable!() };

            if args.emit_json {
                let json_as_str =
                    serde_json::to_string(&module).expect("failed to serialize module to json");
                println!("{json_as_str}")
            }
        }
    }
}
