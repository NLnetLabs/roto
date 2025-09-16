use std::{
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
    sync::Arc,
};

use inetnum::{addr::Prefix, asn::Asn};
use roto_macros::{roto_method, roto_static_method};

use crate::{
    macros::items,
    runtime::items::{IntoItems, Item, Type},
    Runtime,
};

macro_rules! int_docs {
    ($t:ty) => {&{
        #[allow(unused_comparisons)]
        let signed = if <$t>::MIN < 0 { "signed" } else { "unsigned" };
        let bits = <$t>::BITS;
        let min = <$t>::MIN;
        let max = <$t>::MAX;
        format!("The {signed} {bits}-bit integer type\n\nThis type can represent integers from {min} up to (and including) {max}.")
    }};
}

macro_rules! to_string_impl {
    ($t:ty) => {
        items![
            /// Convert this value into a `String`
            fn to_string(x: $t) -> Arc<str> {
                x.to_string().into()
            }
        ]
    };
}

macro_rules! float_docs {
    ($t:ty) => {
        &{
            #[allow(unused_comparisons)]
            let bits = std::mem::size_of::<$t>() * 8;
            format!("The {bits}-bit floating point type")
        }
    };
}

macro_rules! float_impl {
    ($t:ty) => {items! [
        /// Returns the smallest integer greater than or equal to self.
        fn floor(x: $t) -> $t {
            x.floor()
        }

        /// Returns the smallest integer greater than or equal to self.
        fn ceil(x: $t) -> $t {
            x.ceil()
        }

        /// Returns the nearest integer to self. If a value is half-way between two integers, round away from 0.0.
        fn round(x: $t) -> $t {
            x.round()
        }

        /// Computes the absolute value of self.
        fn abs(x: $t) -> $t {
            x.abs()
        }

        /// Returns the square root of a number.
        fn sqrt(x: $t) -> $t {
            x.sqrt()
        }

        /// Raises a number to a floating point power.
        fn pow(x: $t, y: $t) -> $t {
            x.powf(y)
        }

        /// Returns true if this value is NaN.
        fn is_nan(x: $t) -> bool {
            x.is_nan()
        }

        /// Returns true if this value is positive infinity or negative infinity, and false otherwise.
        fn is_infinite(x: $t) -> bool {
            x.is_infinite()
        }

        /// Returns true if this number is neither infinite nor NaN.
        fn is_finite(x: $t) -> bool {
            x.is_finite()
        }
    ]};
}

fn ip_addr_methods() -> Vec<Item> {
    items![
        /// Check whether two IP addresses are equal
        ///
        /// A more convenient but equivalent method for checking equality is via the `==` operator.
        ///
        /// An IPv4 address is never equal to an IPv6 address. IP addresses are considered equal if
        /// all their bits are equal.
        ///
        /// ```roto
        /// 192.0.0.0 == 192.0.0.0   # -> true
        /// ::0 == ::0               # -> true
        /// 192.0.0.0 == 192.0.0.1   # -> false
        /// 0.0.0.0 == 0::0          # -> false
        ///
        /// # or equivalently:
        /// 192.0.0.0.eq(192.0.0.0)  # -> true
        /// ```
        fn ipaddr_eq(a: IpAddr, b: IpAddr) -> bool {
            a == b
        }

        /// Returns true if this address is an IPv4 address, and false otherwise.
        ///
        /// ```roto
        /// 1.1.1.1.is_ipv4() # -> true
        /// ::.is_ipv4()      # -> false
        /// ```
        fn is_ipv4(ip: IpAddr) -> bool {
            ip.is_ipv4()
        }

        /// Returns true if this address is an IPv6 address, and false otherwise.
        ///
        /// ```roto
        /// 1.1.1.1.is_ipv6() # -> false
        /// ::.is_ipv6()      # -> true
        /// ```
        fn is_ipv6(ip: IpAddr) -> bool {
            ip.is_ipv6()
        }

        /// Converts this address to an IPv4 if it is an IPv4-mapped IPv6 address, otherwise it returns self as-is.
        fn to_canonical(ip: IpAddr) -> IpAddr {
            ip.to_canonical()
        }
    ]
}

fn string_methods() -> Vec<Item> {
    items![
        /// Append a string to another, creating a new string
        ///
        /// ```roto
        /// "hello".append(" ").append("world") # -> "hello world"
        /// ```
        fn append(a: Arc<str>, b: Arc<str>) -> Arc<str> {
            format!("{a}{b}").into()
        }

        /// Check whether a string contains another string
        ///
        /// ```roto
        /// "haystack".contains("hay")  # -> true
        /// "haystack".contains("corn") # -> false
        /// ```
        fn contains(haystack: Arc<str>, needle: Arc<str>) -> bool {
            haystack.contains(needle.as_ref())
        }

        /// Check whether a string starts with a given prefix
        ///
        /// ```roto
        /// "haystack".starts_with("hay")   # -> true
        /// "haystack".starts_with("trees") # -> false
        /// ```
        fn starts_with(s: Arc<str>, prefix: Arc<str>) -> bool {
            s.starts_with(prefix.as_ref())
        }

        /// Check whether a string end with a given suffix
        ///
        /// ```roto
        /// "haystack".ends_with("stack") # -> true
        /// "haystack".ends_with("black") # -> false
        /// ```
        fn ends_with(s: Arc<str>, suffix: Arc<str>) -> bool {
            s.ends_with(suffix.as_ref())
        }

        /// Create a new string with all characters converted to lowercase
        ///
        /// ```roto
        /// "LOUD".to_lowercase() # -> "loud"
        /// ```
        fn to_lowercase(s: Arc<str>) -> Arc<str> {
            s.to_lowercase().into()
        }

        /// Create a new string with all characters converted to uppercase
        ///
        /// ```roto
        /// "quiet".to_uppercase() # -> "QUIET"
        /// ```
        fn to_uppercase(s: Arc<str>) -> Arc<str> {
            s.to_uppercase().into()
        }

        /// Repeat a string `n` times and join them
        ///
        /// ```roto
        /// "ha".repeat(6) # -> "hahahahahaha"
        /// ```
        fn repeat(s: Arc<str>, n: u32) -> Arc<str> {
            s.repeat(n as usize).into()
        }

        /// Check for string equality
        fn eq(s: Arc<str>, other: Arc<str>) -> bool {
            s == other
        }
    ]
}

fn built_ins() -> Vec<Item> {
    let mut bool_ty = Type::value::<bool>(
        "bool",
        "The boolean type\n\
            \n\
            This type has two possible values: `true` and `false`. Several \
            boolean operations can be used with booleans, such as `&&` (\
            logical and), `||` (logical or) and `not`.",
    )
    .unwrap();

    // All the integer types
    let mut u8_ty = Type::value::<u8>("u8", int_docs!(u8)).unwrap();
    let mut u16_ty = Type::value::<u16>("u16", int_docs!(u16)).unwrap();
    let mut u32_ty = Type::value::<u32>("u32", int_docs!(u32)).unwrap();
    let mut u64_ty = Type::value::<u64>("u64", int_docs!(u64)).unwrap();
    let mut i8_ty = Type::value::<i8>("i8", int_docs!(i8)).unwrap();
    let mut i16_ty = Type::value::<i16>("i16", int_docs!(i16)).unwrap();
    let mut i32_ty = Type::value::<i32>("i32", int_docs!(i32)).unwrap();
    let mut i64_ty = Type::value::<i64>("i64", int_docs!(i64)).unwrap();
    let mut f32_ty = Type::value::<f32>("f32", float_docs!(f32)).unwrap();
    let mut f64_ty = Type::value::<f64>("f64", float_docs!(f64)).unwrap();

    let mut asn_ty = Type::value::<Asn>(
        "Asn",
        "An ASN: an Autonomous System Number\n\
        \n\
        An AS number can contain a number of 32-bits and is therefore similar to a [`u32`](u32). \
        However, AS numbers cannot be manipulated with arithmetic operations. An AS number \
        is constructed with the `AS` prefix followed by a number.\n\
        \n\
        ```roto\n\
        AS0\n\
        AS1010\n\
        AS4294967295\n\
        ```\n\
        ",
    ).unwrap();
    let mut asn_ty = item! {
        /// An ASN: an Autonomous System Number
        ///
        /// An AS number can contain a number of 32-bits and is therefore similar to a [`u32`](u32)
        /// However, AS numbers cannot be manipulated with arithmetic operations. An AS numbe
        /// is constructed with the `AS` prefix followed by a number.
        ///
        /// ```roto
        /// AS0
        /// AS1010
        /// AS4294967295
        /// ```
        value type Asn = Asn;
    };

    let mut ip_addr_ty = Type::copy::<IpAddr>(
        "IpAddr",
        "An IP address\n\nCan be either IPv4 or IPv6.\n\
        \n\
        For IPv4, only dot-separated quad notation is supported.\n\
        \n\
        ```roto\n\
        # IPv4 examples\n\
        127.0.0.1\n\
        0.0.0.0\n\
        255.255.255.255\n\
        \n\
        # IPv6 examples\n\
        0:0:0:0:0:0:0:1\n\
        ::1\n\
        ::\n\
        ```\n\
        ",
    )
    .unwrap();

    let mut prefix_ty = Type::copy::<Prefix>(
        "Prefix",
        "An IP address prefix: the combination of an IP address and a prefix length\n\n\
        A prefix can be constructed with the `/` operator or with the \
        [`Prefix.new`](Prefix.new) function. This operator takes an [`IpAddr`](IpAddr) \
        and a [`u8`](u8) as operands.\n
        \n\
        ```roto\n\
        1.1.1.0 / 8\n\
        192.0.0.0.0 / 24\n\
        ```\n\
        ",
    ).unwrap();

    let mut string_ty =
        Type::clone::<Arc<str>>("String", "The string type").unwrap();

    bool_ty.add(to_string_impl!(bool));
    u8_ty.add(to_string_impl!(u8));
    u16_ty.add(to_string_impl!(u16));
    u32_ty.add(to_string_impl!(u32));
    u64_ty.add(to_string_impl!(u64));
    i8_ty.add(to_string_impl!(i8));
    i16_ty.add(to_string_impl!(i16));
    i32_ty.add(to_string_impl!(i32));
    i64_ty.add(to_string_impl!(i64));
    f32_ty.add(to_string_impl!(f32));
    f64_ty.add(to_string_impl!(f64));
    ip_addr_ty.add(to_string_impl!(IpAddr));
    prefix_ty.add(to_string_impl!(Prefix));
    asn_ty.add(to_string_impl!(Asn));

    string_ty.add(items![
        /// Convert this value into a `String`
        fn to_string(x: Arc<str>) -> Arc<str> {
            x
        }
    ]);

    f32_ty.add(float_impl!(f32));
    f64_ty.add(float_impl!(f64));

    prefix_ty.add(items![
        /// Construct a new prefix
        ///
        /// A prefix can also be constructed with the `/` operator.
        ///
        /// ```roto
        /// Prefix.new(192.169.0.0, 16)
        ///
        /// # or equivalently
        /// 192.169.0.0 / 16
        /// ```
        fn prefix_new(ip: IpAddr, len: u8) -> Prefix {
            Prefix::new(ip, len).unwrap()
        }
    ]);

    ip_addr_ty.add(ip_addr_methods());

    string_ty.add(string_methods());

    let mut constants = items![
        /// The IPv4 address pointing to localhost: `127.0.0.1`
        const LOCALHOSTV4: IpAddr = IpAddr::from(Ipv4Addr::LOCALHOST);

        /// The IPv6 address pointing to localhost: `::1`
        const LOCALHOSTV6: IpAddr = IpAddr::from(Ipv6Addr::LOCALHOST);
    ];

    let mut types = vec![
        bool_ty.into(),
        u8_ty.into(),
        u16_ty.into(),
        u32_ty.into(),
        u64_ty.into(),
        i8_ty.into(),
        i16_ty.into(),
        i32_ty.into(),
        i64_ty.into(),
        ip_addr_ty.into(),
        asn_ty.into(),
        prefix_ty.into(),
        string_ty.into(),
    ];

    let mut all = Vec::new();
    all.append(&mut constants);
    all.append(&mut types);
    all
}
