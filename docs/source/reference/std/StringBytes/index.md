# StringBytes
`````{roto:type} StringBytes
A view into a string indexed by bytes.
`````


````{roto:function} get(self: StringBytes, idx: u64) -> Option[char]
Get the character at byte offset `idx`.
````

````{roto:function} len(self: StringBytes) -> u64
Get the length of the string in bytes.
````

````{roto:function} list(self: StringBytes) -> List[u8]
Returns the list of bytes of this string
````

````{roto:function} slice(self: StringBytes, i: u64, j: u64) -> Option[String]
Slice this string based on byte indices.

This method returns `None` if either `i` or `j` is out of bounds or if
`i` is greater than `j`.
````

