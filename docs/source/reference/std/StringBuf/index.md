# StringBuf
`````{roto:type} StringBuf
A mutable string type

It is possible to mutate this type in place, allowing for faster
manipulation. In particular, adding `char`s or `String` to the end
of this type is much cheaper than using `+` or `String.append`.
`````


````{roto:function} as_string(self: StringBuf) -> String
Get the underlying `String` of this `StringBuf`
````

````{roto:function} from(s: String) -> StringBuf
Create a `StringBuf` with an initial `String`
````

````{roto:function} new() -> StringBuf
Create a new empty `StringBuf`
````

````{roto:function} push_char(self: StringBuf, c: char)
Add a `char` to the end of this `StringBuf`
````

````{roto:function} push_string(self: StringBuf, s: String)
Add a `String` to the end of this `StringBuf`
````

