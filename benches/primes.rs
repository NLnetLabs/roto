use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use roto::Runtime;

fn is_prime(x: u64) -> bool {
    match x {
        1 => false,
        2 => true,
        n => (2..n).all(|i| n % i != 0),
    }
}

pub fn bench_primes(c: &mut Criterion) {
    let rt = Runtime::new();
    let mut pkg = rt.compile("benches/primes.roto").unwrap();
    let f = pkg.get_function::<fn(u64) -> bool>("main").unwrap();

    let mut group = c.benchmark_group("primes");
    for n in [2, 3, 10, 11, 20, 3203, 3204] {
        assert_eq!(f.call(n).unwrap(), is_prime(n), "{n}");
        group.bench_with_input(
            BenchmarkId::from_parameter(n),
            &n,
            |b, &n| b.iter(|| f.call(std::hint::black_box(n))),
        );
    }
}

criterion_group!(benches, bench_primes);
criterion_main!(benches);
