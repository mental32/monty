use std::path::PathBuf;

fn main() {
    let args: Vec<_> = std::env::args().skip(1).collect();

    if args.len() == 0 {
        eprintln!("usage: montyc-parser <input>");
        std::process::exit(1);
    }

    let mut span_interner = montyc_parser::span_interner::SpanInterner::new();

    let files = args.into_iter().map(|st| {
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
            assert!(module.is_some(), "no module returned");
        }
    }

}