#[macro_export]
macro_rules! yang {
    ($($yang:tt)+) => {
        yang_internal!($($yang)+)
    };
}

#[macro_export]
macro_rules! yang_internal {
    // ($(block::tt)*) => {
    //     yang_internal!($($block:tt)*)
    // };

    ($n1:ident $n2:ident $(block:tt)*) => {
        // println!("type {}", $n1)
        yang_internal!($(block:tt)*)
    };

    // ($block:block) => {
    //     println!("block {:?}", $block)
    // };

    // (typedef $name:ident $($rest:tt)*) => {
    //     yang_internal!($($rest)*)
    // };

    (description $desc:literal) => {

    };


    // ($name:ident) => {
    //     println!("ident {}", $name)
    // };
}

#[macro_export]
macro_rules! allowed_subs {
    ($($kw:ident),*) => {
        vec![$(Keyword::$kw),*]
    };
}
#[macro_export]
macro_rules! allowed_subs2 {
    ($(($kw:ident, $c: ident),)*) => {
        // println!("$kw, $c_mmin, $c_max");

        vec![$((Keyword::$kw, Cardinality::$c)),*]
    };
}
