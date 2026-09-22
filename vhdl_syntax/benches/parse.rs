mod common;

use std::time::Duration;

use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use vhdl_syntax::parser::error::SyntaxErr;
use vhdl_syntax::parser::parse;
use vhdl_syntax::syntax::DesignFileSyntax;

fn parse_all(sources: &[Vec<u8>]) -> Vec<(DesignFileSyntax, Vec<SyntaxErr>)> {
    sources
        .iter()
        .map(|source| parse(source.as_slice()))
        .collect()
}

fn parse_corpora(c: &mut Criterion) {
    let mut group = c.benchmark_group("Tokenize + Parse");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(30));

    for (name, dir) in [
        ("vhdl_libraries", "vhdl_libraries"),
        ("neorv32", "example_project/neorv32"),
    ] {
        let sources = common::corpus(dir);

        group.throughput(Throughput::Bytes(common::total_bytes(&sources)));
        group.bench_function(name, |b| b.iter(|| parse_all(&sources)));
    }

    group.finish();
}

criterion_group!(benches, parse_corpora);
criterion_main!(benches);
