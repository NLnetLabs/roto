use std::{
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
    ptr::NonNull,
    sync::{Arc, Mutex},
};

use inetnum::{addr::Prefix, asn::Asn};

use crate::{
    Library, Val, library,
    runtime::func::OutPtr,
    value::{DynVal, ErasedList, VTable, list::ffi::list_get},
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
        library! {
            impl $t {
                /// Convert this value into a `String`
                fn to_string(self) -> Arc<str> {
                    self.to_string().into()
                }
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
            impl $t {
                /// Returns the smallest integer greater than or equal to self.
                fn floor(self) -> Self {
                    self.floor()
                }

                /// Returns the smallest integer greater than or equal to self.
                fn ceil(self) -> Self {
                    self.ceil()
                }

                /// Returns the nearest integer to self. If a value is half-way between two integers, round away from 0.0.
                fn round(self) -> Self {
                    self.round()
                }

                /// Computes the absolute value of self.
                fn abs(self) -> Self {
                    self.abs()
                }

                /// Returns the square root of a number.
                fn sqrt(self) -> Self {
                    self.sqrt()
                }

                /// Raises a number to a floating point power.
                fn pow(self, exp: Self) -> Self {
                    self.powf(exp)
                }

                /// Returns true if this value is NaN.
                fn is_nan(self) -> bool {
                    self.is_nan()
                }

                /// Returns true if this value is positive infinity or negative infinity, and false otherwise.
                fn is_infinite(self) -> bool {
                    self.is_infinite()
                }

                /// Returns true if this number is neither infinite nor NaN.
                fn is_finite(self) -> bool {
                    self.is_finite()
                }
            }
        }
    };
}

fn ip_addr_methods() -> Library {
    library! {
        impl IpAddr {
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
            fn eq(self, other: IpAddr) -> bool {
                self == other
            }

            /// Returns true if this address is an IPv4 address, and false otherwise.
            ///
            /// ```roto
            /// 1.1.1.1.is_ipv4() # -> true
            /// ::.is_ipv4()      # -> false
            /// ```
            fn is_ipv4(self) -> bool {
                self.is_ipv4()
            }

            /// Returns true if this address is an IPv6 address, and false otherwise.
            ///
            /// ```roto
            /// 1.1.1.1.is_ipv6() # -> false
            /// ::.is_ipv6()      # -> true
            /// ```
            fn is_ipv6(self) -> bool {
                self.is_ipv6()
            }

            /// Converts this address to an IPv4 if it is an IPv4-mapped IPv6 address; otherwise, it returns self as-is.
            fn to_canonical(self) -> IpAddr {
                self.to_canonical()
            }

            /// The IPv4 address pointing to localhost: `127.0.0.1`
            const LOCALHOSTV4: IpAddr = IpAddr::from(Ipv4Addr::LOCALHOST);

            /// The IPv6 address pointing to localhost: `::1`
            const LOCALHOSTV6: IpAddr = IpAddr::from(Ipv6Addr::LOCALHOST);
        }
    }
}

fn string_methods() -> Library {
    library! {
        impl Arc<str> {
            /// Append a string to another, creating a new string
            ///
            /// ```roto
            /// "hello".append(" ").append("world") # -> "hello world"
            /// ```
            fn append(self, other: Self) -> Self {
                format!("{self}{other}").into()
            }

            /// Check whether a string contains another string
            ///
            /// ```roto
            /// "haystack".contains("hay")  # -> true
            /// "haystack".contains("corn") # -> false
            /// ```
            fn contains(self, needle: Self) -> bool {
                self.contains(needle.as_ref())
            }

            /// Check whether a string starts with a given prefix
            ///
            /// ```roto
            /// "haystack".starts_with("hay")   # -> true
            /// "haystack".starts_with("trees") # -> false
            /// ```
            fn starts_with(self, prefix: Self) -> bool {
                self.starts_with(prefix.as_ref())
            }

            /// Check whether a string ends with a given suffix
            ///
            /// ```roto
            /// "haystack".ends_with("stack") # -> true
            /// "haystack".ends_with("black") # -> false
            /// ```
            fn ends_with(self, suffix: Self) -> bool {
                self.ends_with(suffix.as_ref())
            }

            /// Create a new string with all characters converted to lowercase
            ///
            /// ```roto
            /// "LOUD".to_lowercase() # -> "loud"
            /// ```
            fn to_lowercase(self) -> Self {
                self.to_lowercase().into()
            }

            /// Create a new string with all characters converted to uppercase
            ///
            /// ```roto
            /// "quiet".to_uppercase() # -> "QUIET"
            /// ```
            fn to_uppercase(self) -> Self {
                self.to_uppercase().into()
            }

            /// Repeat a string `n` times and join them
            ///
            /// ```roto
            /// "ha".repeat(6) # -> "hahahahahaha"
            /// ```
            fn repeat(self, n: u32) -> Self {
                self.repeat(n as usize).into()
            }

            /// Check for string equality
            fn eq(self, other: Self) -> bool {
                self == other
            }
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

        /// A character in a `String`
        ///
        /// A `char` represents a Unicode code point.
        #[value]
        type char = char;

        /// An ASN: an Autonomous System Number
        ///
        /// An AS number can contain a number of 32-bits and is therefore similar to a [`u32`](u32).
        /// However, AS numbers cannot be manipulated with arithmetic operations. An AS number
        /// is constructed with the `AS` prefix followed by a number.
        ///
        /// Can be used to store both 2-byte and 4-byte ASNs.
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
            fn to_string(self) -> Self {
                self
            }
        }

        include!(to_string_impl!(bool));
        include!(to_string_impl!(u8));
        include!(to_string_impl!(u16));
        include!(to_string_impl!(u32));
        include!(to_string_impl!(u64));
        include!(to_string_impl!(i8));
        include!(to_string_impl!(i16));
        include!(to_string_impl!(i32));
        include!(to_string_impl!(i64));
        include!(to_string_impl!(f32));
        include!(to_string_impl!(f64));
        include!(to_string_impl!(char));
        include!(to_string_impl!(IpAddr));
        include!(to_string_impl!(Prefix));
        include!(to_string_impl!(Asn));

        include!(float_impl!(f32));
        include!(float_impl!(f64));

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
                Prefix::new_relaxed(ip, len).unwrap()
            }

            /// Returns the IP address part of a prefix.
            fn addr(self) -> IpAddr {
                self.addr()
            }

            /// Returns the smallest address of the prefix.
            ///
            /// This is the same as `Prefix.addr`.
            fn min_addr(self) -> IpAddr {
                self.min_addr()
            }

            /// Returns the largest address of the prefix.
            fn max_addr(self) -> IpAddr {
                self.max_addr()
            }

            /// Returns the length part of a prefix.
            fn len(self) -> u8 {
                self.len()
            }

            /// Check whether those prefixes are the same
            fn eq(self, other: Self) -> bool {
                self == other
            }
        }

        include!(ip_addr_methods());
        include!(string_methods());

        /// A mutable string type
        ///
        /// It is possible to mutate this type in place, allowing for faster
        /// manipulation. In particular, adding `char`s or `String` to the end
        /// of this type is much cheaper than using `+` or `String.append`.
        #[clone] type StringBuf = Val<Arc<Mutex<String>>>;

        impl Val<Arc<Mutex<String>>> {
            /// Create a new empty `StringBuf`
            fn new() -> Self {
                Val(Default::default())
            }

            /// Create a `StringBuf` with an initial `String`
            fn from(s: Arc<str>) -> Self {
                Val(Arc::new(Mutex::new(s.as_ref().to_owned())))
            }

            /// Add a `char` to the end of this `StringBuf`
            fn push_char(self, c: char) {
                self.lock().unwrap().push(c);
            }

            /// Add a `String` to the end of this `StringBuf`
            fn push_string(self, s: Arc<str>) {
                self.lock().unwrap().push_str(&s);
            }

            /// Get the underlying `String` of this `StringBuf`
            fn as_string(self) -> Arc<str> {
                let s = self.lock().unwrap();
                (&**s).into()
            }
        }

        /// A growable array
        #[clone] type List<T> = ErasedList;

        impl ErasedList {
            /// Create a new empty list.
            #[sig = "fn[T]() -> List[T]"]
            #[vtables(T)]
            fn new(vtable: VTable) -> Self {
                Self::new(vtable)
            }

            /// Push an element to the end of this list.
            #[sig = "fn[T](List[T], T)"]
            fn push(mut self, elem: DynVal) {
                unsafe { self.push(NonNull::new_unchecked(elem.0)) };
            }

            /// Concatenate this list with another, returning the result.
            ///
            /// The arguments are not mutated by this function.
            #[sig = "fn[T](List[T], List[T]) -> List[T]"]
            fn concat(self, other: Self) -> Self {
                unsafe { self.concat(&other) }
            }

            /// Get an element from the list.
            ///
            /// This function returns `None` if the index is out of bounds.
            #[sig = "fn[T](List[T], u64) -> T?"]
            fn get(out: OutPtr<DynVal>, this: Self, idx: u64) {
                unsafe { list_get(out.ptr.cast(), this, idx) }
            }

            /// Swap two elements in this list at the given indices.
            ///
            /// This function does nothing if either `i` or `j` is out of bounds.
            #[sig = "fn[T](List[T], u64, u64)"]
            fn swap(self, i: u64, j: u64) {
                self.swap(i as usize, j as usize);
            }

            /// Returns the length of this list.
            #[sig = "fn[T](List[T]) -> u64"]
            fn len(self) -> u64 {
                self.len() as u64
            }

            /// Returns the capacity of the current allocation of this list.
            #[sig = "fn[T](List[T]) -> u64"]
            fn capacity(self) -> u64 {
                self.capacity() as u64
            }

            /// Returns whether is list is empty.
            #[sig = "fn[T](List[T]) -> bool"]
            fn is_empty(self) -> bool {
                self.is_empty()
            }
        }
    }
}
