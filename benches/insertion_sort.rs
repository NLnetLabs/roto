use criterion::{
    BatchSize, BenchmarkId, Criterion, criterion_group, criterion_main,
};
use roto::{List, Runtime};

pub fn bench_insertion_sort(c: &mut Criterion) {
    let rt = Runtime::new();
    let mut pkg = rt.compile("benches/insertion_sort.roto").unwrap();
    let fib = pkg
        .get_function::<fn(List<u64>) -> List<u64>>("main")
        .unwrap();

    let mut group = c.benchmark_group("insertion_sort");
    for (name, n) in [
        (
            "1 to 10 scrambled",
            List::from([1, 2, 9, 10, 3, 4, 7, 8, 5, 6]),
        ),
        ("0 to 100", (0..100).collect()),
        ("100 to 0", (0..100).rev().collect()),
    ] {
        // Quick check that the implementation works
        {
            let x = n.clone().into_iter().collect();
            let y = fib.call(x).unwrap();
            let mut prev = 0;
            for elem in y {
                if elem >= prev {
                    prev = elem;
                } else {
                    panic!()
                }
            }
        }

        group.bench_with_input(
            BenchmarkId::from_parameter(name),
            &n,
            |b, n| {
                b.iter_batched(
                    // Just cloning is not enough because lists are by reference.
                    || n.clone().into_iter().collect::<List<_>>(),
                    |n| fib.call(std::hint::black_box(n)),
                    BatchSize::SmallInput,
                )
            },
        );
    }
}

criterion_group!(benches, bench_insertion_sort);
criterion_main!(benches);
