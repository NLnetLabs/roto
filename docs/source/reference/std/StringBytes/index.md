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
Returns the list of bytes of this string.
````

````{roto:function} slice(self: StringBytes, start: u64, end: u64) -> Option[String]
Slice this string based on byte indices.

The byte at index `end` is not included in the returned
string.

This method returns `None` if either `start` or `end` is out of
bounds or if `start` is greater than `end`. Additionally, this
method also returns `None` if the resulting string would not be
valid UTF-8, i.e. when the one of the indices is within a code
point.
````

