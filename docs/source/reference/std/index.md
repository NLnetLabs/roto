# Standard Library


## Types
```{toctree}
:maxdepth: 1
:hidden: 1
Asn <Asn/index>
IpAddr <IpAddr/index>
List <List/index>
Option <Option/index>
Prefix <Prefix/index>
Result <Result/index>
String <String/index>
StringBuf <StringBuf/index>
StringBytes <StringBytes/index>
StringChars <StringChars/index>
StringLines <StringLines/index>
Verdict <Verdict/index>
bool <bool/index>
char <char/index>
f32 <f32/index>
f64 <f64/index>
i8 <i8/index>
i16 <i16/index>
i32 <i32/index>
i64 <i64/index>
u8 <u8/index>
u16 <u16/index>
u32 <u32/index>
u64 <u64/index>
```

:::{list-table}
:class: roto-contents
* - [`Asn`](Asn/index)
  - An ASN: an Autonomous System Number.
* - [`IpAddr`](IpAddr/index)
  - An IP address.
* - [`List`](List/index)
  - A growable array.
* - [`Option`](Option/index)
  - An optional value.
* - [`Prefix`](Prefix/index)
  - An IP address prefix: the combination of an IP address and a prefix length.
* - [`Result`](Result/index)
  - A type that represents either success (`Ok`) or failure (`Err`).
* - [`String`](String/index)
  - The string type.
* - [`StringBuf`](StringBuf/index)
  - A mutable string type.
* - [`StringBytes`](StringBytes/index)
  - A view into a string indexed by bytes.
* - [`StringChars`](StringChars/index)
  - A view into a string indexed by chars.
* - [`StringLines`](StringLines/index)
  - A view into a string indexed by lines.
* - [`Verdict`](Verdict/index)
  - The verdict that a filter reaches about a value, that is, whether to accept or reject it.
* - [`bool`](bool/index)
  - The boolean type.
* - [`char`](char/index)
  - A character in a `String`.
* - [`f32`](f32/index)
  - The 32-bit floating point type.
* - [`f64`](f64/index)
  - The 64-bit floating point type.
* - [`i8`](i8/index)
  - The signed 8-bit integer type.
* - [`i16`](i16/index)
  - The signed 16-bit integer type.
* - [`i32`](i32/index)
  - The signed 32-bit integer type.
* - [`i64`](i64/index)
  - The signed 64-bit integer type.
* - [`u8`](u8/index)
  - The unsigned 8-bit integer type.
* - [`u16`](u16/index)
  - The unsigned 16-bit integer type.
* - [`u32`](u32/index)
  - The unsigned 32-bit integer type.
* - [`u64`](u64/index)
  - The unsigned 64-bit integer type.
:::

## Functions
````{roto:function} print(s: String)
Print a string to stdout
````

