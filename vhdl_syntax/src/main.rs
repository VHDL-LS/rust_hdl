use vhdl_syntax::syntax::{AstNode, DesignFileSyntax};

fn main() {
    /* let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <directory>", args[0]);
        std::process::exit(1);
    } */
    let args = vec![
        "",
        "/Users/lukasscheller/IdeaProjects/rust_hdl/example_project/",
    ];

    check_parsing_and_reemitting(args);

    // check_file("/Users/lukasscheller/IdeaProjects/rust_hdl/example_project/PoC/tb/cache/cache_mem_tb.vhdl");
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
    let text = std::fs::read_to_string(&path).expect("Failed to read file");
    let file = text.parse::<DesignFileSyntax>().expect("Found diagnostics");
    let raw = file.raw();
    assert_eq!(
        raw.to_string(),
        text,
        "File content mismatch for {:?}",
        path
    );
}
