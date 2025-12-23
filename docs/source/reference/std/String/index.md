# String
`````{roto:type} String
The string type

A `String` literal is created by putting some text between double
quotes.

```roto
let x = "Hello!";
```

See [the language reference](#lang_strings) for more information.
Roto supports string formatting when a string literal is prefixed
with an `f`.
`````


````{roto:function} append(self: String, other: String) -> String
Append a string to another, creating a new string

```roto
"hello".append(" ").append("world") # -> "hello world"
```
````

````{roto:function} contains(self: String, needle: String) -> bool
Check whether a string contains another string

```roto
"haystack".contains("hay")  # -> true
"haystack".contains("corn") # -> false
```
````

````{roto:function} ends_with(self: String, suffix: String) -> bool
Check whether a string ends with a given suffix

```roto
"haystack".ends_with("stack") # -> true
"haystack".ends_with("black") # -> false
```
````

````{roto:function} eq(self: String, other: String) -> bool
Check for string equality
````

````{roto:function} repeat(self: String, n: u32) -> String
Repeat a string `n` times and join them

```roto
"ha".repeat(6) # -> "hahahahahaha"
```
````

````{roto:function} starts_with(self: String, prefix: String) -> bool
Check whether a string starts with a given prefix

```roto
"haystack".starts_with("hay")   # -> true
"haystack".starts_with("trees") # -> false
```
````

````{roto:function} to_lowercase(self: String) -> String
Create a new string with all characters converted to lowercase

```roto
"LOUD".to_lowercase() # -> "loud"
```
````

````{roto:function} to_string(self: String) -> String
Convert this value into a `String`
````

````{roto:function} to_uppercase(self: String) -> String
Create a new string with all characters converted to uppercase

```roto
"quiet".to_uppercase() # -> "QUIET"
```
````

