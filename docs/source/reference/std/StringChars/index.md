# StringChars
`````{roto:type} StringChars
A view into a string indexed by chars.
`````


````{roto:function} get(self: StringChars, idx: u64) -> Option[char]
Get the nth character of this string.
````

````{roto:function} len(self: StringChars) -> u64
Get the number of characters in a string.
````

````{roto:function} list(self: StringChars) -> List[char]
Get a list of characters that this string consists of.
````

````{roto:function} slice(self: StringChars, i: u64, j: u64) -> Option[String]
Slice this string based on the character indices.

This method returns `None` if either `i` or `j` is out of bounds or if
`i` is greater than `j`.
````

