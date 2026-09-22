#[allow(unused)]
mod common;

use std::hint::black_box;

use criterion::{
    criterion_group, criterion_main, measurement::WallTime, BenchmarkGroup, Criterion,
};
use vhdl_syntax::{
    latin_1::Latin1String,
    tokens::{Keyword, TokenKind, Tokenize},
};

#[derive(Default)]
struct Words {
    kw_only: Vec<Latin1String>,
    ident_only: Vec<Latin1String>,
    mixed: Vec<Latin1String>,
}

fn words_in_source_order(sources: &[Vec<u8>]) -> Words {
    let mut words = Words::default();
    for (tok, _) in sources.iter().flat_map(|s| s.tokenize()) {
        match tok.kind() {
            TokenKind::Keyword(_) => &mut words.kw_only,
            TokenKind::Identifier => &mut words.ident_only,
            _ => continue,
        }
        .push(tok.text().to_latin1_string());
        words.mixed.push(tok.text().to_latin1_string());
    }
    words
}

fn bench_lookups(
    group: &mut BenchmarkGroup<'_, WallTime>,
    name: impl AsRef<str>,
    words: &[Latin1String],
) {
    group.throughput(criterion::Throughput::Elements(words.len() as u64));
    group.bench_function(name.as_ref(), |b| {
        b.iter(|| {
            for word in words {
                black_box(Keyword::from_latin1(word));
            }
        });
    });
}

fn string_to_corpa(c: &mut Criterion) {
    let mut group = c.benchmark_group("String To Keyword");

    for (name, dir) in [
        ("vhdl_libraries", "vhdl_libraries"),
        ("neorv32", "example_project/neorv32"),
    ] {
        let words = words_in_source_order(&common::corpus(dir));

        bench_lookups(&mut group, format!("{name}/mixed"), &words.mixed);
        bench_lookups(&mut group, format!("{name}/Keyword Only"), &words.kw_only);
        bench_lookups(
            &mut group,
            format!("{name}/Identifier Only"),
            &words.ident_only,
        );
    }
}

criterion_group!(benches, string_to_corpa);
criterion_main!(benches);
