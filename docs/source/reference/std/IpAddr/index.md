# IpAddr
`````{roto:type} IpAddr
An IP address.

Can be either IPv4 or IPv6.

For IPv4, only dot-separated quad notation is supported.

```roto
// IPv4 examples
let a = 127.0.0.1;
let b = 0.0.0.0;
let c = 255.255.255.255;

// IPv6 examples
let d = 0:0:0:0:0:0:0:1;
let e = ::1;
let f = ::;
```
`````


````{roto:function} eq(self: IpAddr, other: IpAddr) -> bool
Check whether two IP addresses are equal.

A more convenient but equivalent method for checking equality is via the `==` operator.

An IPv4 address is never equal to an IPv6 address. IP addresses are considered equal if
all their bits are equal.

```roto
let a = 192.0.0.0 == 192.0.0.0;   // -> true
let b = ::0 == ::0;               // -> true
let c = 192.0.0.0 == 192.0.0.1;   // -> false
let d = 0.0.0.0 == 0::0;          // -> false

// or equivalently:
let e = 192.0.0.0.eq(192.0.0.0);  // -> true
```
````

````{roto:function} is_ipv4(self: IpAddr) -> bool
Returns true if this address is an IPv4 address, and false otherwise.

```roto
let a = 1.1.1.1.is_ipv4(); // -> true
let b = ::.is_ipv4();      // -> false
```
````

````{roto:function} is_ipv6(self: IpAddr) -> bool
Returns true if this address is an IPv6 address, and false otherwise.

```roto
let a = 1.1.1.1.is_ipv6(); // -> false
let b = ::.is_ipv6();      // -> true
```
````

````{roto:function} to_canonical(self: IpAddr) -> IpAddr
Converts this address to an IPv4 if it is an IPv4-mapped IPv6 address; otherwise, it returns self as-is.
````

````{roto:function} to_string(self: IpAddr) -> String
Convert this value into a `String`.
````

