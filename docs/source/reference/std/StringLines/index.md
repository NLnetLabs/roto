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
Get a list of lines.
````

````{roto:function} slice(self: StringLines, start: u64, end: u64) -> Option[String]
Slice this string by lines.

The line at index `end` is not included in the returned string.

This method returns `None` if either `start` or `end` is out of
bounds or if `start` is greater than `end`.
````

