macro_rules! register {
    ( $runtime:ident $(,)? ) => {};
    (
        $runtime:ident,

        $(#[doc = $docstring:expr])*
        fn $name:ident($($arg:ident: $a:ty),*$(,)?) $(-> $r:ty)? $body:block

        $($rest:tt)*
    ) => {
        fn $name($($arg: $a),*) $(-> $r)? $body

        $runtime.register_fn(
            stringify!($name),
            {
                vec![$(stringify!($docstring)),*].join("\n")
            },
            [$(stringify!($arg)),*],
            $name,
        ).unwrap();

        register! {
            $runtime,

            $($rest)*
        }
    };
    (
        $runtime:ident,

        $(#[doc = $docstring:expr])*
        let $name:ident = |$($arg:ident: $a:ty),*$(,)?| $(-> $r:ty)? $body:block

        $($rest:tt)*
    ) => {
        $runtime.register_fn(
            stringify!($name),
            {
                vec![$(stringify!($docstring)),*].join("\n")
            },
            [$(stringify!($arg)),*],
            |$($arg: $a),*| $(-> $r)? { $body }
        ).unwrap();

        register! {
            $runtime,

            $($rest)*
        }
    };
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use crate::Runtime;

    #[test]
    fn register_with_macro() {
        let mut runtime = Runtime::new();

        register! {
            runtime,

            /// This is a docstring for foo
            fn foo() {
                println!("foo");
            }

            /// This is a docstring for bar
            fn bar(a: i32) -> Arc<str> {
                format!("bar: {a}").into()
            }

            /// This is a docstring for baz
            let baz = |a: i32| -> Arc<str> {
                let s = format!("baz: {a}");
                s.as_str().into()
            }
        }
    }
}
