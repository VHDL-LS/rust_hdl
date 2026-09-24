mod common;

use std::{hint::black_box, time::Duration};

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use vhdl_syntax::{parser::parse, syntax::DesignFileSyntax};

fn walk_nodes_sequentially(tree: &[DesignFileSyntax]) {
    tree.iter().for_each(|file| {
        for node in file.descendants() {
            black_box(node);
        }
    });
}

fn walk_nodes_parallel(tree: &[DesignFileSyntax]) {
    tree.par_iter().for_each(|file| {
        for node in file.descendants() {
            black_box(node);
        }
    });
}

fn walk_corpora(c: &mut Criterion) {
    let mut group = c.benchmark_group("Walk Corpora");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(30));

    for (name, dir) in [
        ("vhdl_libraries", "vhdl_libraries"),
        ("neorv32", "example_project/neorv32"),
    ] {
        let sources = common::corpus(dir);
        let files = sources
            .iter()
            .map(|source| parse(source.as_slice()).0)
            .collect::<Vec<_>>();

        group.throughput(Throughput::Bytes(common::total_bytes(&sources)));
        group.bench_function(format!("{name}/sequential"), |b| {
            b.iter(|| walk_nodes_sequentially(&files))
        });
        for threads in [1, 2, 4, 8] {
            let pool = rayon::ThreadPoolBuilder::new()
                .num_threads(threads)
                .build()
                .unwrap();
            group.bench_with_input(
                BenchmarkId::new(format!("{name}/parallel"), threads),
                &files,
                |b, d| {
                    b.iter(|| pool.install(|| walk_nodes_parallel(d)));
                },
            );
        }
    }

    group.finish();
}

criterion_group!(benches, walk_corpora);
criterion_main!(benches);
