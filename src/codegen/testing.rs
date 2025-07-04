use super::{Module, TypedFunc};
use crate::Verdict;

pub struct TestCase<Ctx: 'static> {
    name: String,
    func: TypedFunc<Ctx, fn() -> Verdict<(), ()>>,
}

impl<Ctx: 'static> TestCase<Ctx> {
    pub fn new(
        name: String,
        func: TypedFunc<Ctx, fn() -> Verdict<(), ()>>,
    ) -> Self {
        Self { name, func }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn run(&self, ctx: &mut Ctx) -> Result<(), ()> {
        match self.func.call(ctx) {
            Verdict::Accept(()) => Ok(()),
            Verdict::Reject(()) => Err(()),
        }
    }
}

pub(crate) fn get_tests<Ctx: 'static>(
    module: &mut Module,
) -> impl Iterator<Item = TestCase<Ctx>> + use<'_, Ctx> {
    let tests: Vec<_> = module
        .functions
        .keys()
        .filter(|x| {
            x.rsplit_once(".")
                .map_or(x.as_ref(), |x| x.1)
                .starts_with("test#")
        })
        .map(Clone::clone)
        .collect();

    tests.into_iter().map(|name| {
        TestCase::new(
            name.replace("test#", ""),
            module
                .get_function::<Ctx, fn() -> Verdict<(), ()>>(
                    name.strip_prefix("pkg.").unwrap(),
                )
                .unwrap(),
        )
    })
}

pub(crate) fn run_tests<Ctx: 'static>(
    module: &mut Module,
    mut ctx: Ctx,
) -> Result<(), ()> {
    let tests: Vec<_> = get_tests::<Ctx>(module).collect();

    let total = tests.len();
    let total_width = total.to_string().len();
    let mut successes = 0;
    let mut failures = 0;

    for (n, test) in tests.into_iter().enumerate() {
        let n = n + 1;
        let test_display = test.name();
        print!("Test {n:>total_width$} / {total}: {test_display}... ");

        if test.run(&mut ctx) == Ok(()) {
            successes += 1;
            println!("\x1B[92mok\x1B[m");
        } else {
            failures += 1;
            println!("\x1B[91mfail\x1B[m");
        }
    }
    println!("Ran {total} tests, {successes} succeeded, {failures} failed");

    if failures == 0 {
        Result::Ok(())
    } else {
        Result::Err(())
    }
}
