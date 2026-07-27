use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use roto::Runtime;

pub fn bench_fibonacci(c: &mut Criterion) {
    let rt = Runtime::new();
    let mut pkg = rt.compile("benches/fibonacci.roto").unwrap();
    let fib = pkg.get_function::<fn(u64) -> u64>("main").unwrap();

    let mut group = c.benchmark_group("fib");
    for n in [1, 10, 20] {
        group.bench_with_input(
            BenchmarkId::from_parameter(n),
            &n,
            |b, &n| b.iter(|| fib.call(std::hint::black_box(n))),
        );
    }
}

criterion_group!(benches, bench_fibonacci);
criterion_main!(benches);
