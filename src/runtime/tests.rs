#![allow(unused_imports)]

use std::sync::Arc;

use crate::{library, runtime::items::Registerable as _, Val};

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
    let _ = library! {
        fn accept() -> bool {
            true
        }
    };
}

#[test]
#[should_panic]
fn invalid_method_name() {
    let _ = library! {
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
    let _ = library! {
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

    let _ = library! {
        #[clone] type accept = Val<Foo>;
    };
}

#[test]
#[should_panic]
fn invalid_copy_type_name() {
    #[derive(Clone, Copy)]
    struct Foo;

    let _ = library! {
        #[copy] type accept = Val<Foo>;
    };
}

#[test]
#[should_panic]
fn invalid_constant_name() {
    let _ = library! {
        const accept: u32 = 0;
    };
}

#[test]
fn constant_declared_twice() {
    Runtime::from_lib(library! {
        const FOO: u32 = 10;
        const FOO: u32 = 12;
    })
    .unwrap_err();
}

#[test]
fn function_declared_twice() {
    Runtime::from_lib(library! {
        fn foo() {}
        fn foo() -> bool {
            false
        }
    })
    .unwrap_err();
}

#[test]
fn method_declared_twice() {
    Runtime::from_lib(library! {
        fn foo(_: bool) {}
        fn foo(_: bool) -> bool {
            false
        }
    })
    .unwrap_err();
}

#[test]
fn static_method_declared_twice() {
    Runtime::from_lib(library! {
        fn foo() {}
        fn foo() -> bool {
            false
        }
    })
    .unwrap_err();
}

#[test]
fn method_and_static_method_with_the_same_name() {
    Runtime::from_lib(library! {
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
    Runtime::from_lib(library! {
        fn foo(_: bool) {}

        impl bool {
            fn foo(_: bool) -> bool { false }
        }
    })
    .unwrap();
}

#[test]
fn function_and_constant_with_the_same_name_1() {
    Runtime::from_lib(library! {
        fn foo(_: bool) {}
        const foo: bool = true;
    })
    .unwrap_err();
}

#[test]
fn function_and_constant_with_the_same_name_2() {
    Runtime::from_lib(library! {
        const foo: bool = true;
        fn foo(_x: bool) {}
    })
    .unwrap_err();
}

#[test]
#[should_panic]
fn register_option_arc_str() {
    // Cannot register Option
    let _ = library! {
        #[clone] type OptStr = Option<Arc<str>>;
    };
}

#[test]
fn register_val_option_arc_str() {
    // But with Val it's fine
    Runtime::from_lib(library! {
        #[clone] type OptStr = Val<Option<Arc<str>>>;
    })
    .unwrap();
}

// This is a bit of a weird case, it should probably at least warn, but
// at the moment, this is perfectly valid (where unwrap_or_empty is a
// static method and not a method).
#[test]
fn unwrap_or_empty() {
    let ty = library! {
        #[clone] type OptStr = Val<Option<Arc<str>>>;

        impl Val<Option<Arc<str>>> {
            fn unwrap_or_empty(x: Option<Arc<str>>) -> Arc<str> {
                x.unwrap_or_default()
            }
        }
    };

    Runtime::from_lib(ty).unwrap();
}

#[allow(deprecated)]
mod deprecated_api {
    use roto_macros::roto_method;

    use crate::{src, Runtime, Val};

    #[test]
    fn use_the_old_api() {
        #[derive(Clone)]
        struct Foo(u64);

        let mut rt = Runtime::new();

        rt.register_clone_type::<Val<Foo>>("...").unwrap();

        #[roto_method(rt, Val<Foo>)]
        fn double(Val(x): Val<Foo>) -> Val<Foo> {
            Val(Foo(x.0 * 2))
        }

        let s = src!(
            "
            fn bar(x: Foo) -> Foo {
                x.double()
            }      
        "
        );

        let mut pkg = s
            .parse()
            .and_then(|x| x.typecheck(&rt))
            .map(|x| {
                let x = x.lower_to_mir().lower_to_lir();
                x.codegen()
            })
            .unwrap();

        let bar = pkg
            .get_function::<(), fn(Val<Foo>) -> Val<Foo>>("bar")
            .unwrap();
        let res = bar.call(&mut (), Val(Foo(5)));
        assert_eq!(res.0 .0, 10);
    }
}
