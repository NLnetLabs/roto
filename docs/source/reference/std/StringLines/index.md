# StringLines
`````{roto:type} StringLines
A view into a string indexed by lines.
`````


````{roto:method} StringLines.get(self: StringLines, idx: u64) -> Option[char]
Get the nth line in this string.
````

````{roto:method} StringLines.len(self: StringLines) -> u64
Get the number of lines in this string.
````

````{roto:method} StringLines.list(self: StringLines) -> List[String]
Get a list of lines.
````

````{roto:method} StringLines.slice(self: StringLines, start: u64, end: u64) -> Option[String]
Slice this string by lines.

The line at index `end` is not included in the returned string.

This method returns `None` if either `start` or `end` is out of
bounds or if `start` is greater than `end`.
````

