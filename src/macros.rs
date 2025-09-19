macro_rules! items {
    ( $($tokens:tt)* ) => {{
        let mut vec = Vec::<$crate::runtime::items::Item>::new();
        $crate::macros::items_inner!(vec, $($tokens)*);
        vec
    }};
}

macro_rules! items_inner {
    ( $vec:ident, ) => {};
    (
        $vec:ident,

        $(#[doc = $docstring:expr])*
        copy type $name:ident = $ty:ty;

        $($rest:tt)*
    ) => {
        $vec.push($crate::runtime::item::Item::from($crate::runtime::items::Type::copy::<$ty>(
            stringify($name),
            vec![$($docstring),*].join("\n"),
        )));

        $crate::macros::items_inner! {
            $vec,
            $($rest)*
        }
    };
    (
        $vec:ident,

        $(#[doc = $docstring:expr])*
        clone type $name:ident = $ty:ty;

        $($rest:tt)*
    ) => {
        $vec.push($crate::runtime::item::Item::from($crate::runtime::items::Type::clone::<$ty>(
            stringify($name),
            vec![$($docstring),*].join("\n"),
        )));

        $crate::macros::items_inner! {
            $vec,
            $($rest)*
        }
    };
    (
        $vec:ident,

        $(#[doc = $docstring:expr])*
        value type $name:ident = $ty:ty;

        $($rest:tt)*
    ) => {
        $vec.push($crate::runtime::item::Item::from($crate::runtime::items::Type::value::<$ty>(
            stringify($name),
            vec![$($docstring),*].join("\n"),
        )));

        $crate::macros::items_inner! {
            $vec,
            $($rest)*
        }
    };
    (
        $vec:ident,

        $(#[doc = $docstring:expr])*
        const $name:ident: $ty:ty = $val:expr;

        $($rest:tt)*
    ) => {
        $vec.push($crate::runtime::items::Item::from($crate::runtime::items::Constant::new(
            stringify!($name),
            vec![$($docstring),*].join("\n"),
            $val,
        ).unwrap()));

        $crate::macros::items_inner! {
            $vec,
            $($rest)*
        }
    };
    (
        $vec:ident,

        $(#[doc = $docstring:expr])*
        fn $name:ident($($arg:ident: $a:ty),*$(,)?) $(-> $r:ty)? $body:block

        $($rest:tt)*
    ) => {{
        $vec.push({
            fn $name($($arg: $a),*) $(-> $r)? $body

            $crate::runtime::items::Item::from($crate::runtime::items::Function::new(
                stringify!($name),
                vec![$($docstring),*].join("\n"),
                { let x: Vec<&'static str> = vec![$(stringify!($arg)),*]; x},
                $name,
            ).unwrap())
        });

        $crate::macros::items_inner! {
            $vec,
            $($rest)*
        }
    }};
    (
        $vec:ident,

        $(#[doc = $docstring:expr])*
        let $name:ident = |$($arg:ident: $a:ty),*$(,)?| $(-> $r:ty)? $body:block;

        $($rest:tt)*
    ) => {{
        $vec.push($crate::runtime::items::Item::from($crate::runtime::items::Function::new(
            stringify!($name),
            {
                vec![$($docstring),*].join("\n")
            },
            { let x: Vec<&'static str> = vec![$(stringify!($arg)),*]; x},
            |$($arg: $a),*| $(-> $r)? { $body }
        ).unwrap()));

        $crate::macros::items_inner! { $vec, $($rest)* }
    }};
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    #[test]
    fn register_with_macro() {
        dbg!(items! {
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
            };
        });
        panic!();
    }
}

pub(crate) use items;

pub(crate) use items_inner;
