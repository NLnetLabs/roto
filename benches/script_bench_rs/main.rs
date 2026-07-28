//! This benchmark is adapted from https://github.com/khvzak/script-bench-rs.
//!
//! Any changes to this benchmark should be upstreamed into the
//! `script-bench-rs` repository.
//!
//! That is a benchmark comparing embedded scripting languages for Rust. It
//! aims specifically to test the integration with Rust, which is why we
//! define `RustData` in Rust, instead of just using a `RotoString` directly.

use criterion::{Criterion, criterion_group, criterion_main};
use rand::RngExt;
use roto::{Library, List, RotoString, Runtime, Val, library};

#[derive(Clone, Default, PartialEq)]
pub struct RustData(pub RotoString);

fn benchmark(c: &mut Criterion) {
    let rt = Runtime::from_lib(lib()).unwrap();
    let mut pkg = rt
        .compile("benches/script_bench_rs/sort_userdata.roto")
        .unwrap();
    let f = pkg
        .get_function::<fn() -> List<Val<RustData>>>("main")
        .unwrap();

    validate(f.call());

    c.bench_function("Sort Rust objects", |b| b.iter(|| f.call()));
}

fn validate(list: List<Val<RustData>>) {
    // Validate that the results are sorted
    let mut count = 0;
    let mut prev = RustData::default();
    list.into_iter().for_each(|next| {
        let next = next.0;
        assert!(*prev.0 <= *next.0);
        prev = next.clone();
        count += 1;
    });

    assert_eq!(count, 10000);
}

fn lib() -> Library {
    library! {
        fn rand(n: u64) -> u64 {
            rand::rng().random_range(0..n)
        }

        impl RotoString {
            fn get(self, idx: u64) -> RotoString {
                self[idx as usize..idx as usize + 1].into()
            }

            fn len(self) -> u64 {
                self.bytes().len() as u64
            }
        }

        #[clone] type RustData = Val<RustData>;

        impl Val<RustData> {
            fn new(s: RotoString) -> Val<RustData> {
                Val(RustData(s))
            }

            fn lt(this: Val<RustData>, rhs: Val<RustData>) -> bool {
                *this.0.0 < *rhs.0.0
            }
        }
    }
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = benchmark
}

criterion_main!(benches);
