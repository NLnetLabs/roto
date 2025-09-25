#![allow(unused_imports)]

use std::sync::Arc;

use crate::{items, runtime::items::IntoItems as _, Val};

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
    items! {
        fn accept() -> bool {
            true
        }
    };
}

#[test]
#[should_panic]
fn invalid_method_name() {
    items! {
        impl bool {
            fn accept(_x: bool) -> bool {
                true
            }
        }
    };
}

#[test]
#[should_panic]
fn invalid_static_method_name() {
    items! {
        impl bool {
            fn accept() -> bool {
                true
            }
        }
    };
}

#[test]
#[should_panic]
fn invalid_clone_type_name() {
    #[derive(Clone)]
    struct Foo;

    items! {
        clone type accept = Val<Foo>;
    };
}

#[test]
#[should_panic]
fn invalid_copy_type_name() {
    #[derive(Clone, Copy)]
    struct Foo;

    items! {
        copy type accept = Val<Foo>;
    };
}

#[test]
#[should_panic]
fn invalid_constant_name() {
    items! {
        const accept: u32 = 0;
    };
}

#[test]
fn constant_declared_twice() {
    Runtime::from_items(items! {
        const FOO: u32 = 10;
        const FOO: u32 = 12;
    })
    .unwrap_err();
}

#[test]
fn function_declared_twice() {
    Runtime::from_items(items! {
        fn foo() {}
        fn foo() -> bool {
            false
        }
    })
    .unwrap_err();
}

#[test]
fn method_declared_twice() {
    Runtime::from_items(items! {
        fn foo(_: bool) {}
        fn foo(_: bool) -> bool {
            false
        }
    })
    .unwrap_err();
}

#[test]
fn static_method_declared_twice() {
    Runtime::from_items(items! {
        fn foo() {}
        fn foo() -> bool {
            false
        }
    })
    .unwrap_err();
}

#[test]
fn method_and_static_method_with_the_same_name() {
    Runtime::from_items(items! {
        impl bool {
            fn foo(_: bool) {}

            fn foo() -> bool {
                false
            }
        }
    })
    .unwrap_err();
}

#[test]
fn function_and_method_with_the_same_name() {
    Runtime::from_items(items! {
        fn foo(_: bool) {}

        impl bool {
            fn foo(_: bool) -> bool { false }
        }
    })
    .unwrap();
}

#[test]
fn function_and_constant_with_the_same_name_1() {
    Runtime::from_items(items! {
        fn foo(_: bool) {}
        const foo: bool = true;
    })
    .unwrap_err();
}

#[test]
fn function_and_constant_with_the_same_name_2() {
    Runtime::from_items(items! {
        const foo: bool = true;
        fn foo(_x: bool) {}
    })
    .unwrap_err();
}

#[test]
#[should_panic]
fn register_option_arc_str() {
    // Cannot register Option
    items! {
        clone type OptStr = Option<Arc<str>>;
    };
}

#[test]
fn register_val_option_arc_str() {
    // But with Val it's fine
    Runtime::from_items(items! {
        clone type OptStr = Val<Option<Arc<str>>>;
    })
    .unwrap();
}

#[test]
fn unwrap_or_empty() {
    let ty = items! {
        clone type OptStr = Val<Option<Arc<str>>>;

        impl Val<Option<Arc<str>>> {
            fn unwrap_or_empty(x: Option<Arc<str>>) -> Arc<str> {
                x.unwrap_or_default()
            }
        }
    };

    Runtime::from_items(ty).unwrap_err();
}
