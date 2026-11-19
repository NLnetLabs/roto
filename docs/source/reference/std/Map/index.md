# Map
`````{roto:type} Map
A map from one set of value to another.
`````


````{roto:method} Map.contains(self: Map[K, V], key: K) -> bool
Returns whether the map contains a certain key.
````

````{roto:method} Map.get(self: Map[K, V], key: K) -> Option[V]
Get a value from the map.

This function returns `None` if the key is not in the map.
````

````{roto:method} Map.insert(self: Map[K, V], key: K, value: V)
Insert a new value into the map.

If the key is already present in the map, its value is overwritten.
````

````{roto:method} Map.new() -> Map[K, V]
Create a new empty map.
````

