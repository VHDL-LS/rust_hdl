mod common;

use std::time::Duration;

use brunch::{benches, Bench};
use vhdl_syntax::parser::error::SyntaxErr;
use vhdl_syntax::parser::parse;
use vhdl_syntax::syntax::DesignFileSyntax;

fn parse_all(sources: &[Vec<u8>]) -> Vec<(DesignFileSyntax, Vec<SyntaxErr>)> {
    sources
        .iter()
        .map(|source| parse(source.as_slice()))
        .collect()
}

fn main() {
    let vhdl_libraries = common::corpus("vhdl_libraries");
    let neorv32 = common::corpus("example_project/neorv32");

    // Warm up the interner and allocate all symbols:
    // we only want to measure parse time here.
    drop(parse_all(&vhdl_libraries));
    drop(parse_all(&neorv32));

    benches!(
        inline:

        Bench::new("Tokenize + Parse: vhdl_libraries")
            .with_timeout(Duration::from_secs(30))
            .run(|| parse_all(&vhdl_libraries)),

        Bench::new("Tokenize + Parse: neorv32")
            .with_timeout(Duration::from_secs(30))
            .run(|| parse_all(&neorv32)),
    );
}
