use std::hint::black_box;

use criterion::{criterion_group, criterion_main, Criterion};
use vhdl_syntax::latin_1::Latin1String;

fn latin1_cmp(c: &mut Criterion) {
    let mut group = c.benchmark_group("Latin1 compare");

    let ascii = Latin1String::from_utf8("std_logic_vector").unwrap();
    let non_ascii = Latin1String::from_utf8("signal_café_ÄÖü").unwrap();
    let upper = Latin1String::from_utf8("STD_LOGIC_VECTOR").unwrap();

    group.bench_function("eq_str/ascii", |b| {
        b.iter(|| black_box(&ascii) == black_box("std_logic_vector"))
    });
    group.bench_function("eq_str/non_ascii", |b| {
        b.iter(|| black_box(&non_ascii) == black_box("signal_café_ÄÖü"))
    });
    group.bench_function("ne_str/ascii", |b| {
        b.iter(|| black_box(&ascii) == black_box("std_logic_vectoR"))
    });
    group.bench_function("eq_ignore_case/ascii", |b| {
        b.iter(|| black_box(&ascii).eq_ignore_case(black_box(&upper)))
    });
    group.bench_function("to_string/ascii", |b| {
        b.iter(|| black_box(&ascii).to_string())
    });
    group.finish();
}

criterion_group!(benches, latin1_cmp);
criterion_main!(benches);
