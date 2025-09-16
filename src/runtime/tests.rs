#![allow(unused_imports)]

use std::sync::Arc;

use crate::Val;

use super::Runtime;
use roto_macros::{roto_function, roto_method, roto_static_method};
use routecore::bgp::{
    aspath::{AsPath, HopPath},
    communities::{Community, Wellknown},
    types::{LocalPref, OriginType},
};

pub fn routecore_runtime() -> Result<Runtime, String> {
    let mut rt = Runtime::new();

    // rt.register_clone_type::<Val<OriginType>>("TODO")?;
    // rt.register_clone_type::<Val<LocalPref>>("TODO")?;
    // rt.register_clone_type::<Val<Community>>("TODO")?;
    // rt.register_clone_type::<Val<HopPath>>("TODO")?;
    // rt.register_clone_type::<Val<AsPath<Vec<u8>>>>("TODO")?;

    // #[roto_function(rt)]
    // fn pow(x: u32, y: u32) -> u32 {
    //     x.pow(y)
    // }

    // #[roto_method(rt, u32)]
    // fn is_even(x: u32) -> bool {
    //     x % 2 == 0
    // }

    // rt.register_constant(
    //     "BLACKHOLE",
    //     "The well-known BLACKHOLE community.",
    //     Val(Community::from(Wellknown::Blackhole)),
    // )
    // .unwrap();

    Ok(rt)
}

#[test]
#[should_panic]
fn invalid_function_name() {
    let rt = Runtime::new();

    // #[roto_function(rt)]
    fn accept() -> bool {
        true
    }
}

#[test]
#[should_panic]
fn invalid_method_name() {
    let rt = Runtime::new();

    // #[roto_method(rt, bool)]
    fn accept(_x: bool) -> bool {
        true
    }
}

#[test]
#[should_panic]
fn invalid_static_method_name() {
    let rt = Runtime::new();

    // #[roto_static_method(rt, bool)]
    fn accept() -> bool {
        true
    }
}

#[test]
fn invalid_type_name() {
    let rt = Runtime::new();

    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy)]
    struct accept;

    // rt.register_clone_type::<Val<accept>>("").unwrap_err();
    // rt.register_copy_type::<Val<accept>>("").unwrap_err();
}

#[test]
fn invalid_constant_name() {
    let mut rt = Runtime::new();

    rt.register_constant("FOO$BAR", "...", 10u32).unwrap_err();
}

#[test]
fn constant_declared_twice() {
    let mut rt = Runtime::new();

    rt.register_constant("FOO", "...", 10u32).unwrap();
    rt.register_constant("FOO", "...", 10u32).unwrap_err();
}

#[test]
#[should_panic]
fn function_declared_twice() {
    let rt = Runtime::new();

    // #[roto_function(rt)]
    fn foo() {}

    // #[roto_function(rt, foo)]
    fn foo2() -> bool {
        false
    }
}

#[test]
#[should_panic]
fn method_declared_twice() {
    let rt = Runtime::new();

    // #[roto_method(rt, bool, foo)]
    fn foo1(_: bool) {}

    // #[roto_method(rt, bool, foo)]
    fn foo2(_: bool) -> bool {
        false
    }
}

#[test]
#[should_panic]
fn static_method_declared_twice() {
    let rt = Runtime::new();

    // #[roto_static_method(rt, bool, foo)]
    fn foo1() {}

    // #[roto_static_method(rt, bool, foo)]
    fn foo2() -> bool {
        false
    }
}

#[test]
#[should_panic]
fn method_and_static_method_with_the_same_name() {
    let rt = Runtime::new();

    // #[roto_method(rt, bool, foo)]
    fn foo1(_: bool) {}

    // #[roto_static_method(rt, bool, foo)]
    fn foo2() -> bool {
        false
    }
}

#[test]
fn function_and_method_with_the_same_name() {
    let rt = Runtime::new();

    // #[roto_function(rt, foo)]
    fn foo1(_: bool) {}

    // #[roto_method(rt, bool, foo)]
    fn foo2(_: bool) -> bool {
        false
    }
}

#[test]
fn function_and_constant_with_the_same_name_1() {
    let mut rt = Runtime::new();

    // #[roto_function(rt, foo)]
    fn foo1(_: bool) {}

    rt.register_constant("foo", "...", true).unwrap_err();
}

#[test]
#[should_panic]
fn function_and_constant_with_the_same_name_2() {
    let mut rt = Runtime::new();

    rt.register_constant("foo", "...", true).unwrap();

    // #[roto_function(rt, foo)]
    fn foo1(_: bool) {}
}

#[test]
fn register_option_arc_str() {
    let runtime = Runtime::new();

    // Cannot register Option
    // runtime
    //     .register_clone_type_with_name::<Option<Arc<str>>>("OptStr", "")
    //     .unwrap_err();

    // But with Val it's fine
    // runtime
    //     .register_clone_type_with_name::<Val<Option<Arc<str>>>>("OptStr", "")
    //     .unwrap();
}

#[test]
#[should_panic]
fn unwrap_or_empty() {
    let runtime = Runtime::new();

    // runtime
    //     .register_clone_type_with_name::<Val<Option<Arc<str>>>>("OptStr", "")
    //     .unwrap();

    // This should panic because the argument is missing `Val<_>`
    // #[roto_method(runtime, Val<Option<Arc<str>>>)]
    fn unwrap_or_empty(x: Option<Arc<str>>) -> Arc<str> {
        x.unwrap_or_default()
    }
}
