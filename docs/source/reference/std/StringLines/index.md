# StringLines
`````{roto:type} StringLines
A view into a string indexed by lines.
`````


````{roto:function} get(self: StringLines, idx: u64) -> Option[char]
Get the nth line in this string.
````

````{roto:function} len(self: StringLines) -> u64
Get the number of lines in this string.
````

````{roto:function} list(self: StringLines) -> List[String]
Get a list of lines
````

````{roto:function} slice(self: StringLines, i: u64, j: u64) -> Option[String]
Slice this string by lines.

This method returns `None` if either `i` or `j` is out of bounds or if
`i` is greater than `j`.
````

