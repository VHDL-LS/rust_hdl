use std::{fs::File, io::Read, path::PathBuf};

use vhdl_syntax::{
    self,
    latin_1::Latin1String,
    parser::Parser,
    tokens::{IntoTokenStream, Tokenize},
};

// PSL is not supported yet by vhdl_syntax
const EXCLUDED_FILES: [&str; 1] = ["example_project/vunit/examples/vhdl/array_axis_vcs/src/fifo.vhd"];

fn check_file(path: impl Into<std::path::PathBuf>) {
    let path = path.into();
    if EXCLUDED_FILES.iter().any(|f| path.ends_with(f)) {
        return;
    }
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
        println!("{}", Latin1String::from(buf));
        println!();
        println!("========= Reconstructed =========");
        println!();
        println!("{}", Latin1String::from(expected_buf));
        println!();
        panic!()
    }
}

#[test]
fn parse_and_re_emit_example_project_files() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path = path.parent().unwrap().to_path_buf();
    path.push("example_project");

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

    visit_dirs(&path);
}
