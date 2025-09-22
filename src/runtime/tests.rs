#![allow(unused_imports)]

use std::sync::Arc;

use crate::{item, runtime::items::IntoItems as _, Val};

use super::Runtime;
use roto_macros::{roto_function, roto_method, roto_static_method};
use routecore::bgp::{
    aspath::{AsPath, HopPath},
    communities::{Community, Wellknown},
    types::{LocalPref, OriginType},
};

#[test]
#[should_panic]
fn invalid_function_name() {
    let _ = Runtime::from_items(item! {
        fn accept() -> bool {
            true
        }
    });
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
#[should_panic]
fn invalid_clone_type_name() {
    #[derive(Clone)]
    struct Foo;

    item! {
        clone type accept = Val<Foo>;
    };
}

#[test]
#[should_panic]
fn invalid_copy_type_name() {
    #[derive(Clone, Copy)]
    struct Foo;

    item! {
        copy type accept = Val<Foo>;
    };
}

#[test]
#[should_panic]
fn invalid_constant_name() {
    item! {
        const accept: u32 = 0;
    };
}

#[test]
fn constant_declared_twice() {
    Runtime::from_items([
        item! {
            const FOO: u32 = 10;
        },
        item! {
            const FOO: u32 = 12;
        },
    ])
    .unwrap_err();
}

#[test]
fn function_declared_twice() {
    Runtime::from_items([
        item! {
            fn foo() {}
        },
        item! {
            fn foo() -> bool {
                false
            }
        },
    ])
    .unwrap_err();
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
fn function_and_constant_with_the_same_name_2() {
    Runtime::from_items([
        item! {
            const foo: bool = true;
        }
        .into_items(),
        item! {
            fn foo1(_x: bool) {}
        }
        .into_items(),
    ])
    .unwrap_err();
}

#[test]
fn register_option_arc_str() {
    // Cannot register Option
    Runtime::from_items(item! {
        clone type OptStr = Option<Arc<str>>;
    })
    .unwrap_err();

    // But with Val it's fine
    Runtime::from_items(item! {
        clone type OptStr = Val<Option<Arc<str>>>;
    })
    .unwrap();
}

#[test]
fn unwrap_or_empty() {
    let mut ty = item! {
        clone type OptStr = Val<Option<Arc<str>>>;
    };

    ty.add(item! {
        fn unwrap_or_empty(x: Option<Arc<str>>) -> Arc<str> {
            x.unwrap_or_default()
        }
    });

    Runtime::from_items(ty).unwrap_err();
}
