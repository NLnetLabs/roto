# List[T]
`````{roto:type} List[T]
A growable array.
`````


````{roto:method} List.capacity(self: List[T]) -> u64
Returns the capacity of the current allocation of this list.
````

````{roto:method} List.concat(self: List[T], other: List[T]) -> List[T]
Concatenate this list with another, returning the result.

The arguments are not mutated by this function.
````

````{roto:method} List.contains(self: List[T], item: T) -> bool
Returns `true` if this list contains the given item, and `false` otherwise.
````

````{roto:method} List.get(self: List[T], idx: u64) -> Option[T]
Get an element from the list.

This function returns `None` if the index is out of bounds.
````

````{roto:method} List.index(self: List[T], item: T) -> Option[u64]
Returns the index of the first occurrence of the given item in
this list, or `None` if the item is not found.
````

````{roto:method} List.is_empty(self: List[T]) -> bool
Returns whether is list is empty.
````

````{roto:method} List.join(self: List[String], separator: String) -> String
Join the strings in a list into a single string.
````

````{roto:method} List.len(self: List[T]) -> u64
Returns the length of this list.
````

````{roto:method} List.new() -> List[T]
Create a new empty list.
````

````{roto:method} List.push(self: List[T], elem: T)
Append an element to the end of this list.
````

````{roto:method} List.swap(self: List[T], i: u64, j: u64)
Swap two elements in this list at the given indices.

This function does nothing if either `i` or `j` is out of bounds.
````

