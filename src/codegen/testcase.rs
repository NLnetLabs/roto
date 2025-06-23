use super::TypedFunc;
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
