mod common;

use std::time::Duration;

use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use vhdl_syntax::tokens::tokenizer::{LexErr, Tokenize};
use vhdl_syntax::tokens::Token;

fn tokenize_all(sources: &[Vec<u8>]) -> Vec<Vec<(Token, Option<LexErr>)>> {
    sources
        .iter()
        .map(|source| source.tokenize().collect())
        .collect()
}

fn tokenize_corpora(c: &mut Criterion) {
    let mut group = c.benchmark_group("Tokenizer");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(30));

    for (name, dir) in [
        ("vhdl_libraries", "vhdl_libraries"),
        ("neorv32", "example_project/neorv32"),
    ] {
        let sources = common::corpus(dir);

        group.throughput(Throughput::Bytes(common::total_bytes(&sources)));
        group.bench_function(name, |b| b.iter(|| tokenize_all(&sources)));
    }

    group.finish();
}

criterion_group!(benches, tokenize_corpora);
criterion_main!(benches);
