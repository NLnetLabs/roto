use std::{
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
    sync::Arc,
};

use inetnum::{addr::Prefix, asn::Asn};
use roto_macros::{roto_method, roto_static_method};

use crate::Runtime;

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
    ($rt:ident, $t:ty) => {{
        /// Convert this value into a `String`
        #[roto_method($rt, $t, to_string)]
        fn to_string(x: $t) -> Arc<str> {
            x.to_string().into()
        }
    }};
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
    ($rt:ident, $t:ty) => {{
        $rt.register_method::<$t, _, _>(
            "floor",
            "Returns the largest integer less than or equal to self",
            ["x"],
            <$t>::floor,
        ).unwrap();

        $rt.register_method::<$t, _, _>(
            "ceil",
            "Returns the smallest integer greater than or equal to self.",
            ["x"],
            <$t>::ceil,
        ).unwrap();

        /// Returns the nearest integer to self. If a value is half-way between two integers, round away from 0.0.
        #[roto_method($rt, $t, round)]
        fn round(x: $t) -> $t {
            x.round()
        }

        /// Computes the absolute value of self.
        #[roto_method($rt, $t, abs)]
        fn abs(x: $t) -> $t {
            x.abs()
        }

        /// Returns the square root of a number.
        #[roto_method($rt, $t, sqrt)]
        fn sqrt(x: $t) -> $t {
            x.sqrt()
        }

        /// Raises a number to a floating point power.
        #[roto_method($rt, $t, pow)]
        fn pow(x: $t, y: $t) -> $t {
            x.powf(y)
        }

        /// Returns true if this value is NaN.
        #[roto_method($rt, $t, is_nan)]
        fn is_nan(x: $t) -> bool {
            x.is_nan()
        }

        /// Returns true if this value is positive infinity or negative infinity, and false otherwise.
        #[roto_method($rt, $t, is_infinite)]
        fn is_infinite(x: $t) -> bool {
            x.is_infinite()
        }

        /// Returns true if this number is neither infinite nor NaN.
        #[roto_method($rt, $t, is_finite)]
        fn is_finite(x: $t) -> bool {
            x.is_finite()
        }
    }};
}

impl Runtime {
    /// A Runtime that is as empty as possible.
    ///
    /// This contains only type information for Roto primitives.
    pub fn new() -> Self {
        let mut rt = Runtime {
            context: None,
            types: Default::default(),
            functions: Default::default(),
            constants: Default::default(),
        };

        rt.register_value_type::<bool>(
            "The boolean type\n\n\
            This type has two possible values: `true` and `false`. Several \
            boolean operations can be used with booleans, such as `&&` (\
            logical and), `||` (logical or) and `not`.",
        )
        .unwrap();

        // All the integer types
        rt.register_value_type::<u8>(int_docs!(u8)).unwrap();
        rt.register_value_type::<u16>(int_docs!(u16)).unwrap();
        rt.register_value_type::<u32>(int_docs!(u32)).unwrap();
        rt.register_value_type::<u64>(int_docs!(u64)).unwrap();
        rt.register_value_type::<i8>(int_docs!(i8)).unwrap();
        rt.register_value_type::<i16>(int_docs!(i16)).unwrap();
        rt.register_value_type::<i32>(int_docs!(i32)).unwrap();
        rt.register_value_type::<i64>(int_docs!(i64)).unwrap();
        rt.register_value_type::<f32>(float_docs!(f32)).unwrap();
        rt.register_value_type::<f64>(float_docs!(f64)).unwrap();

        rt.register_value_type::<Asn>(
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
            ").unwrap();

        rt.register_copy_type::<IpAddr>(
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

        rt.register_copy_type::<Prefix>(
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

        rt.register_clone_type_with_name::<Arc<str>>(
            "String",
            "The string type",
        )
        .unwrap();

        to_string_impl!(rt, bool);
        to_string_impl!(rt, u8);
        to_string_impl!(rt, u16);
        to_string_impl!(rt, u32);
        to_string_impl!(rt, u64);
        to_string_impl!(rt, i8);
        to_string_impl!(rt, i16);
        to_string_impl!(rt, i32);
        to_string_impl!(rt, i64);
        to_string_impl!(rt, f32);
        to_string_impl!(rt, f64);
        to_string_impl!(rt, IpAddr);
        to_string_impl!(rt, Prefix);
        to_string_impl!(rt, Asn);

        /// Convert this value into a `String`
        #[roto_method(rt, Arc<str>, to_string)]
        fn to_string(x: Arc<str>) -> Arc<str> {
            x
        }

        float_impl!(rt, f32);
        float_impl!(rt, f64);

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
        #[roto_static_method(rt, Prefix, new)]
        fn prefix_new(ip: IpAddr, len: u8) -> Prefix {
            Prefix::new(ip, len).unwrap()
        }

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
        #[roto_method(rt, IpAddr, eq)]
        fn ipaddr_eq(a: IpAddr, b: IpAddr) -> bool {
            a == b
        }

        /// Returns true if this address is an IPv4 address, and false otherwise.
        ///
        /// ```roto
        /// 1.1.1.1.is_ipv4() # -> true
        /// ::.is_ipv4()      # -> false
        /// ```
        #[roto_method(rt, IpAddr)]
        fn is_ipv4(ip: IpAddr) -> bool {
            ip.is_ipv4()
        }

        /// Returns true if this address is an IPv6 address, and false otherwise.
        ///
        /// ```roto
        /// 1.1.1.1.is_ipv6() # -> false
        /// ::.is_ipv6()      # -> true
        /// ```
        #[roto_method(rt, IpAddr)]
        fn is_ipv6(ip: IpAddr) -> bool {
            ip.is_ipv6()
        }

        /// Converts this address to an IPv4 if it is an IPv4-mapped IPv6 address, otherwise it returns self as-is.
        #[roto_method(rt, IpAddr)]
        fn to_canonical(ip: IpAddr) -> IpAddr {
            ip.to_canonical()
        }

        rt.register_constant(
            "LOCALHOSTV4",
            "The IPv4 address pointing to localhost: `127.0.0.1`",
            IpAddr::from(Ipv4Addr::LOCALHOST),
        )
        .unwrap();

        rt.register_constant(
            "LOCALHOSTV6",
            "The IPv6 address pointing to localhost: `::1`",
            IpAddr::from(Ipv6Addr::LOCALHOST),
        )
        .unwrap();

        /// Append a string to another, creating a new string
        ///
        /// ```roto
        /// "hello".append(" ").append("world") # -> "hello world"
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn append(a: Arc<str>, b: Arc<str>) -> Arc<str> {
            format!("{a}{b}").into()
        }

        /// Check whether a string contains another string
        ///
        /// ```roto
        /// "haystack".contains("hay")  # -> true
        /// "haystack".contains("corn") # -> false
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn contains(haystack: Arc<str>, needle: Arc<str>) -> bool {
            haystack.contains(needle.as_ref())
        }

        /// Check whether a string starts with a given prefix
        ///
        /// ```roto
        /// "haystack".starts_with("hay")   # -> true
        /// "haystack".starts_with("trees") # -> false
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn starts_with(s: Arc<str>, prefix: Arc<str>) -> bool {
            s.starts_with(prefix.as_ref())
        }

        /// Check whether a string end with a given suffix
        ///
        /// ```roto
        /// "haystack".ends_with("stack") # -> true
        /// "haystack".ends_with("black") # -> false
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn ends_with(s: Arc<str>, suffix: Arc<str>) -> bool {
            s.ends_with(suffix.as_ref())
        }

        /// Create a new string with all characters converted to lowercase
        ///
        /// ```roto
        /// "LOUD".to_lowercase() # -> "loud"
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn to_lowercase(s: Arc<str>) -> Arc<str> {
            s.to_lowercase().into()
        }

        /// Create a new string with all characters converted to uppercase
        ///
        /// ```roto
        /// "quiet".to_uppercase() # -> "QUIET"
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn to_uppercase(s: Arc<str>) -> Arc<str> {
            s.to_uppercase().into()
        }

        /// Repeat a string `n` times and join them
        ///
        /// ```roto
        /// "ha".repeat(6) # -> "hahahahahaha"
        /// ```
        #[roto_method(rt, Arc<str>)]
        fn repeat(s: Arc<str>, n: u32) -> Arc<str> {
            s.repeat(n as usize).into()
        }

        /// Check for string equality
        #[roto_method(rt, Arc<str>)]
        fn eq(s: Arc<str>, other: Arc<str>) -> bool {
            s == other
        }

        rt
    }
}
