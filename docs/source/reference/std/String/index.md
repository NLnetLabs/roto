# String
`````{roto:type} String
The string type
`````


````{roto:function} append(a: String, b: String) -> String
Append a string to another, creating a new string

```roto
"hello".append(" ").append("world") # -> "hello world"
```
````

````{roto:function} contains(haystack: String, needle: String) -> bool
Check whether a string contains another string

```roto
"haystack".contains("hay")  # -> true
"haystack".contains("corn") # -> false
```
````

````{roto:function} ends_with(s: String, suffix: String) -> bool
Check whether a string end with a given suffix

```roto
"haystack".ends_with("stack") # -> true
"haystack".ends_with("black") # -> false
```
````

````{roto:function} eq(s: String, other: String) -> bool
Check for string equality
````

````{roto:function} repeat(s: String, n: u32) -> String
Repeat a string `n` times and join them

```roto
"ha".repeat(6) # -> "hahahahahaha"
```
````

````{roto:function} starts_with(s: String, prefix: String) -> bool
Check whether a string starts with a given prefix

```roto
"haystack".starts_with("hay")   # -> true
"haystack".starts_with("trees") # -> false
```
````

````{roto:function} to_lowercase(s: String) -> String
Create a new string with all characters converted to lowercase

```roto
"LOUD".to_lowercase() # -> "loud"
```
````

````{roto:function} to_string(x: String) -> String
Convert this value into a `String`
````

````{roto:function} to_uppercase(s: String) -> String
Create a new string with all characters converted to uppercase

```roto
"quiet".to_uppercase() # -> "QUIET"
```
````

