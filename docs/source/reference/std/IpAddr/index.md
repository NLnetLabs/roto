# IpAddr
`````{roto:type} IpAddr
An IP address

Can be either IPv4 or IPv6.

For IPv4, only dot-separated quad notation is supported.

```roto
# IPv4 examples
127.0.0.1
0.0.0.0
255.255.255.255

# IPv6 examples
0:0:0:0:0:0:0:1
::1
::
```
`````


````{roto:function} eq(a: IpAddr, b: IpAddr) -> bool
Check whether two IP addresses are equal

A more convenient but equivalent method for checking equality is via the `==` operator.

An IPv4 address is never equal to an IPv6 address. IP addresses are considered equal if
all their bits are equal.

```roto
192.0.0.0 == 192.0.0.0   # -> true
::0 == ::0               # -> true
192.0.0.0 == 192.0.0.1   # -> false
0.0.0.0 == 0::0          # -> false

# or equivalently:
192.0.0.0.eq(192.0.0.0)  # -> true
```
````

````{roto:function} is_ipv4(ip: IpAddr) -> bool
Returns true if this address is an IPv4 address, and false otherwise.

```roto
1.1.1.1.is_ipv4() # -> true
::.is_ipv4()      # -> false
```
````

````{roto:function} is_ipv6(ip: IpAddr) -> bool
Returns true if this address is an IPv6 address, and false otherwise.

```roto
1.1.1.1.is_ipv6() # -> false
::.is_ipv6()      # -> true
```
````

````{roto:function} to_canonical(ip: IpAddr) -> IpAddr
Converts this address to an IPv4 if it is an IPv4-mapped IPv6 address; otherwise, it returns self as-is.
````

````{roto:function} to_string(x: IpAddr) -> String
Convert this value into a `String`
````

