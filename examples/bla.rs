use roto::{roto_function, Runtime};

struct Foo<'a> {
    _x: &'a u32,
}

fn main() {
    let mut rt = Runtime::basic().unwrap();
    rt.register_type::<Foo>().unwrap();

    #[roto_function(rt)]
    fn funcy(_foo: *const Foo) {}

    // rt.register_method::<Foo,_, _>("funcy", funcy as extern "C" fn(_, _)).unwrap();
}
