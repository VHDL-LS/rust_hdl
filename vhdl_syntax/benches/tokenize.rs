mod common;

use std::time::Duration;

use brunch::{benches, Bench};
use vhdl_syntax::tokens::tokenizer::{LexErr, Tokenize};
use vhdl_syntax::tokens::Token;

fn tokenize_all(sources: &[Vec<u8>]) -> Vec<Vec<(Token, Option<LexErr>)>> {
    sources
        .iter()
        .map(|source| source.tokenize().collect())
        .collect()
}

fn main() {
    let vhdl_libraries = common::corpus("vhdl_libraries");
    let neorv32 = common::corpus("example_project/neorv32");

    // Warm up the interner and allocate all symbols:
    // we only want to measure tokenization time here.
    drop(tokenize_all(&vhdl_libraries));
    drop(tokenize_all(&neorv32));

    benches!(
        inline:

        Bench::new("Tokenizer: vhdl_libraries")
            .with_timeout(Duration::from_secs(30))
            .run(|| tokenize_all(&vhdl_libraries)),

        Bench::new("Tokenizer: neorv32")
            .with_timeout(Duration::from_secs(30))
            .run(|| tokenize_all(&neorv32)),
    );
}
