# StringChars
`````{roto:type} StringChars
A view into a string indexed by chars.
`````


````{roto:method} StringChars.get(self: StringChars, idx: u64) -> Option[char]
Get the nth character of this string.
````

````{roto:method} StringChars.len(self: StringChars) -> u64
Get the number of characters in a string.
````

````{roto:method} StringChars.list(self: StringChars) -> List[char]
Get a list of characters that this string consists of.
````

````{roto:method} StringChars.slice(self: StringChars, start: u64, end: u64) -> Option[String]
Slice this string based on the character indices.

The character at index `end` is not included in the returned
string.

This method returns `None` if either `start` or `end` is out of
bounds or if `start` is greater than `end`.
````

