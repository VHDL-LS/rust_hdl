use std::{fs::File, io::Read};

use vhdl_syntax::{
    parser::Parser,
    tokens::{IntoTokenStream, Tokenize},
};

fn main() {
    /* let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <directory>", args[0]);
        std::process::exit(1);
    } */
    let args = vec!["", "/Users/lukasscheller/rust_hdl/example_project/"];

    check_parsing_and_reemitting(args);
}

fn check_parsing_and_reemitting(args: Vec<&'static str>) {
    fn visit_dirs(dir: &std::path::Path) {
        for entry in std::fs::read_dir(dir).expect("Failed to read directory") {
            let entry = entry.expect("Failed to read directory entry");
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path);
            } else if matches!(
                path.extension().and_then(|s| s.to_str()),
                Some("vhd" | "vhdl")
            ) {
                check_file(path);
            }
        }
    }

    visit_dirs(std::path::Path::new(&args[1]));
}

fn check_file(path: impl Into<std::path::PathBuf>) {
    let path = path.into();
    println!("looking at file {}", path.display());
    let mut buf = Vec::new();
    File::open(&path)
        .expect("Failed to open file")
        .read_to_end(&mut buf)
        .expect("Failed to read file");
    let parser = Parser::new(buf.tokenize().into_token_stream());
    let (file, diagnostics) = parser.parse_design_file();
    assert!(
        diagnostics.is_empty(),
        "Found diagnostics for file {}",
        path.display()
    );
    let mut expected_buf = Vec::new();
    file.write_to(&mut expected_buf)
        .expect("Cannot write to vec");
    if buf != expected_buf {
        println!("File content mismatch for {}", path.display());
        println!("========= Original =========");
        println!();
        println!("{:?}", buf);
        println!();
        println!("========= Reconstructed =========");
        println!();
        println!("{:?}", expected_buf);
        println!();
        panic!()
    }
}
