# List[T]
`````{roto:type} List[T]
A growable array
`````


````{roto:function} capacity(self: List[T]) -> u64
Returns the capacity of the current allocation of this list.
````

````{roto:function} concat(self: List[T], other: List[T]) -> List[T]
Concatenate this list with another, returning the result.

The arguments are not mutated by this function.
````

````{roto:function} get(self: List[T], idx: u64) -> Option[T]
Get an element from the list.

This function returns `None` if the index is out of bounds.
````

````{roto:function} is_empty(self: List[T]) -> bool
Returns whether is list is empty.
````

````{roto:function} len(self: List[T]) -> u64
Returns the length of this list.
````

````{roto:function} new() -> List[T]
Create a new empty list.
````

````{roto:function} push(self: List[T], elem: T)
Push an element to the end of this list.
````

````{roto:function} swap(self: List[T], i: u64, j: u64)
Swap two elements in this list at the given indices.

This function does nothing if either `i` or `j` is out of bounds.
````

