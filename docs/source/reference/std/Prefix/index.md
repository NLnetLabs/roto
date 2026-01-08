# Prefix
`````{roto:type} Prefix
An IP address prefix: the combination of an IP address and a prefix length

A prefix can be constructed with the `/` operator or with the
[`Prefix.new`](Prefix.new) function. This operator takes an [`IpAddr`](IpAddr)
and a [`u8`](u8) as operands.

```roto
1.1.1.0 / 8
192.0.0.0.0 / 24
```
`````


````{roto:function} addr(self: Prefix) -> IpAddr
Returns the IP address part of a prefix.
````

````{roto:function} eq(self: Prefix, other: Prefix) -> bool
Check whether those prefixes are the same
````

````{roto:function} len(self: Prefix) -> u8
Returns the length part of a prefix.
````

````{roto:function} max_addr(self: Prefix) -> IpAddr
Returns the largest address of the prefix.
````

````{roto:function} min_addr(self: Prefix) -> IpAddr
Returns the smallest address of the prefix.

This is the same as `Prefix.addr`.
````

````{roto:function} new(ip: IpAddr, len: u8) -> Prefix
Construct a new prefix

A prefix can also be constructed with the `/` operator.

```roto
Prefix.new(192.169.0.0, 16)

# or equivalently
192.169.0.0 / 16
```
````

````{roto:function} to_string(self: Prefix) -> String
Convert this value into a `String`
````

