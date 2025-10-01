# Asn
`````{roto:type} Asn
An ASN: an Autonomous System Number

An AS number can contain a number of 32-bits and is therefore similar to a [`u32`](u32)
However, AS numbers cannot be manipulated with arithmetic operations. An AS number
is constructed with the `AS` prefix followed by a number.

```roto
AS0
AS1010
AS4294967295
```
`````


````{roto:function} to_string(x: Asn) -> String
Convert this value into a `String`
````

