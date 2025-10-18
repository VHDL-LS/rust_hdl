use vhdl_syntax::syntax::{AstNode, DesignFileSyntax};

fn main() {
    let text = std::fs::read_to_string(&std::env::args().nth(1).expect("No file path provided"))
        .expect("Failed to read file");
    let file = text.parse::<DesignFileSyntax>().expect("Erroneous input");
    println!("file: {}", file.raw());
}
