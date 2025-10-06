use std::{
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
    sync::Arc,
};

use inetnum::{addr::Prefix, asn::Asn};

use crate::{library, Library};

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
        library! {
            /// Convert this value into a `String`
            fn to_string(x: $t) -> Arc<str> {
                x.to_string().into()
            }
        }
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
    ($t:ty) => {
        library! {
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
        }
    };
}

fn ip_addr_methods() -> Library {
    library! {
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
        fn eq(a: IpAddr, b: IpAddr) -> bool {
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

        /// Converts this address to an IPv4 if it is an IPv4-mapped IPv6 address; otherwise, it returns self as-is.
        fn to_canonical(ip: IpAddr) -> IpAddr {
            ip.to_canonical()
        }
    }
}

fn string_methods() -> Library {
    library! {
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
    }
}

pub fn built_ins() -> Library {
    library! {
        use Option::{Some, None};

        /// The boolean type
        ///
        /// This type has two possible values: `true` and `false`. Several
        /// boolean operations can be used with booleans, such as `&&` (
        /// logical and), `||` (logical or) and `not`.
        #[value]
        type bool = bool;

        #[doc = int_docs!(u8)]
        #[value]
        type u8 = u8;

        #[doc = int_docs!(u16)]
        #[value]
        type u16 = u16;

        #[doc = int_docs!(u32)]
        #[value]
        type u32 = u32;

        #[doc = int_docs!(u64)]
        #[value]
        type u64 = u64;

        #[doc = int_docs!(i8)]
        #[value]
        type i8 = i8;

        #[doc = int_docs!(i16)]
        #[value]
        type i16 = i16;

        #[doc = int_docs!(i32)]
        #[value]
        type i32 = i32;

        #[doc = int_docs!(i64)]
        #[value]
        type i64 = i64;

        #[doc = float_docs!(f32)]
        #[value]
        type f32 = f32;

        #[doc = float_docs!(f64)]
        #[value]
        type f64 = f64;

        /// An ASN: an Autonomous System Number
        ///
        /// An AS number can contain a number of 32-bits and is therefore similar to a [`u32`](u32)
        /// However, AS numbers cannot be manipulated with arithmetic operations. An AS number
        /// is constructed with the `AS` prefix followed by a number.
        ///
        /// ```roto
        /// AS0
        /// AS1010
        /// AS4294967295
        /// ```
        #[value] type Asn = Asn;

        /// An IP address
        ///
        /// Can be either IPv4 or IPv6.
        ///
        /// For IPv4, only dot-separated quad notation is supported.
        ///
        /// ```roto
        /// # IPv4 examples
        /// 127.0.0.1
        /// 0.0.0.0
        /// 255.255.255.255
        ///
        /// # IPv6 examples
        /// 0:0:0:0:0:0:0:1
        /// ::1
        /// ::
        /// ```
        #[copy] type IpAddr = IpAddr;

        /// An IP address prefix: the combination of an IP address and a prefix length
        ///
        /// A prefix can be constructed with the `/` operator or with the
        /// [`Prefix.new`](Prefix.new) function. This operator takes an [`IpAddr`](IpAddr)
        /// and a [`u8`](u8) as operands.
        ///
        /// ```roto
        /// 1.1.1.0 / 8
        /// 192.0.0.0.0 / 24
        /// ```
        #[copy] type Prefix = Prefix;

        /// The string type
        #[clone] type String = Arc<str>;

        impl Arc<str> {
            /// Convert this value into a `String`
            fn to_string(x: Arc<str>) -> Arc<str> {
                x
            }
        }

        impl bool { include!(to_string_impl!(bool)); }
        impl u8 { include!(to_string_impl!(u8)); }
        impl u16 { include!(to_string_impl!(u16)); }
        impl u32 { include!(to_string_impl!(u32)); }
        impl u64 { include!(to_string_impl!(u64)); }
        impl i8 { include!(to_string_impl!(i8)); }
        impl i16 { include!(to_string_impl!(i16)); }
        impl i32 { include!(to_string_impl!(i32)); }
        impl i64 { include!(to_string_impl!(i64)); }
        impl f32 { include!(to_string_impl!(f32)); }
        impl f64 { include!(to_string_impl!(f64)); }
        impl IpAddr { include!(to_string_impl!(IpAddr)); }
        impl Prefix { include!(to_string_impl!(Prefix)); }
        impl Asn { include!(to_string_impl!(Asn)); }

        impl f32 { include!(float_impl!(f32)); }
        impl f64 { include!(float_impl!(f64)); }

        impl Prefix {
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
            fn new(ip: IpAddr, len: u8) -> Prefix {
                Prefix::new(ip, len).unwrap()
            }
        }

        impl IpAddr { include!(ip_addr_methods()); }
        impl Arc<str> { include!(string_methods()); }

        /// The IPv4 address pointing to localhost: `127.0.0.1`
        const LOCALHOSTV4: IpAddr = IpAddr::from(Ipv4Addr::LOCALHOST);

        /// The IPv6 address pointing to localhost: `::1`
        const LOCALHOSTV6: IpAddr = IpAddr::from(Ipv6Addr::LOCALHOST);

        /// Mathematical constants and functions
        mod math {
            const PI: f64 = std::f64::consts::PI;

            /// The sine of the radian argument x.
            fn sin(x: f64) -> f64 {
                x.sin()
            }

            /// The cosine of the radian argument x.
            fn cos(x: f64) -> f64 {
                x.cos()
            }
        }
    }
}
