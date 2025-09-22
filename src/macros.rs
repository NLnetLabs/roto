#[macro_export]
macro_rules! item {
    (
        $(#[doc = $docstring:expr])*
        copy type $name:ident = $ty:ty;
    ) => {
        $crate::Type::copy::<$ty>(
            stringify!($name),
            {
                let v: Vec<&'static str> = vec![$($docstring),*];
                v.join("\n")
            },
        ).unwrap()
    };
    (
        $(#[doc = $docstring:expr])*
        clone type $name:ident = $ty:ty;
    ) => {
        $crate::Type::clone::<$ty>(
            stringify!($name),
            {
                let v: Vec<&'static str> = vec![$($docstring),*];
                v.join("\n")
            },
        ).unwrap()
    };
    (
        $(#[doc = $docstring:expr])*
        value type $name:ident = $ty:ty;
    ) => {
        $crate::Type::value::<$ty>(
            stringify!($name),
            {
                let v: Vec<&'static str> = vec![$($docstring),*];
                v.join("\n")
            },
        ).unwrap()
    };
    (
        $(#[doc = $docstring:expr])*
        const $name:ident: $ty:ty = $val:expr;
    ) => {
        $crate::Constant::new(
            stringify!($name),
            {
                let v: Vec<&'static str> = vec![$($docstring),*];
                v.join("\n")
            },
            $val,
        ).unwrap()
    };
    (
        $(#[doc = $docstring:expr])*
        fn $name:ident($($args:tt)*) $(-> $r:ty)? $body:block
    ) => {{
        {
            fn $name($($args)*) $(-> $r)? $body

            $crate::Function::new(
                stringify!($name),
                {
                    let v: Vec<&'static str> = vec![$($docstring),*];
                    v.join("\n")
                },
                $crate::args!($($args)*),
                $name,
            ).unwrap()
        }
    }};
    (
        $(#[doc = $docstring:expr])*
        let $name:ident = move || $(-> $r:ty)? $body:block;
    ) => {
        $crate::Function::new(
            stringify!($name),
            {
                let v: Vec<&'static str> = vec![$($docstring),*];
                v.join("\n")
            },
            $crate::args!(),
            move || $(-> $r)? { $body }
        ).unwrap()
    };
    (
        $(#[doc = $docstring:expr])*
        let $name:ident = || $(-> $r:ty)? $body:block;
    ) => {
        $crate::Function::new(
            stringify!($name),
            {
                let v: Vec<&'static str> = vec![$($docstring),*];
                v.join("\n")
            },
            $crate::args!(),
            move || $(-> $r)? { $body }
        ).unwrap()
    };
    (
        $(#[doc = $docstring:expr])*
        let $name:ident = move |$($args:ident: $t:ty),*| $(-> $r:ty)? $body:block;
    ) => {
        $crate::Function::new(
            stringify!($name),
            {
                let v: Vec<&'static str> = vec![$($docstring),*];
                v.join("\n")
            },
            $crate::args!($($args: $t),*),
            move |$($args: $t),*| $(-> $r)? { $body }
        ).unwrap()
    };
    (
        $(#[doc = $docstring:expr])*
        let $name:ident = |$($args:ident: $t:ty),*| $(-> $r:ty)? $body:block;
    ) => {
        $crate::Function::new(
            stringify!($name),
            {
                let v: Vec<&'static str> = vec![$($docstring),*];
                v.join("\n")
            },
            $crate::args!($($args: $t),*),
            |$($args: $t),*| $(-> $r)? { $body }
        ).unwrap()
    };
}

// Combining the rules here seems to lead to problems, not sure why.
#[macro_export]
macro_rules! args {
    ($($arg:ident: $a:ty),*$(,)?) => {
        {
            { let x: Vec<&'static str> = vec![$(stringify!($arg)),*]; x}
        }
    };
    ($(mut $arg:ident: $a:ty),*$(,)?) => {
        {
            { let x: Vec<&'static str> = vec![$(stringify!($arg)),*]; x}
        }
    };
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    #[test]
    fn register_with_macro() {
        dbg!(item! {
            /// This is a docstring for foo
            fn foo() {
                println!("foo");
            }
        });

        dbg!(item! {
            /// This is a docstring for bar
            fn bar(a: i32) -> Arc<str> {
                format!("bar: {a}").into()
            }
        });

        dbg!(item! {
            /// This is a docstring for baz
            let baz = |a: i32| -> Arc<str> {
                let s = format!("baz: {a}");
                s.as_str().into()
            };
        });
        panic!();
    }
}

pub use args;
pub use item;
