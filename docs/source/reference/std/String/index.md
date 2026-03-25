# String
`````{roto:type} String
The string type

A `String` literal is created by putting some text between double
quotes.

```{code-block} roto
:notest:

let x = "Hello!";
```

See [the language reference](#lang_strings) for more information.
Roto supports string formatting when a string literal is prefixed
with an `f`.
`````


````{roto:function} append(self: String, other: String) -> String
Append a string to another, creating a new string.

```{code-block} roto
:notest:

"hello".append(" ").append("world") # -> "hello world"
```
````

````{roto:function} bytes(self: String) -> StringBytes
Get a view of this string indexed by bytes.
````

````{roto:function} chars(self: String) -> StringChars
Get a view of this string indexed by chars.
````

````{roto:function} contains(self: String, needle: String) -> bool
Check whether a string contains another string.

```{code-block} roto
:notest:

"haystack".contains("hay")  # -> true
"haystack".contains("corn") # -> false
```
````

````{roto:function} ends_with(self: String, suffix: String) -> bool
Check whether a string ends with a given suffix.

```{code-block} roto
:notest:

"haystack".ends_with("stack") # -> true
"haystack".ends_with("black") # -> false
```
````

````{roto:function} eq(self: String, other: String) -> bool
Check for string equality.
````

````{roto:function} from_chars(chars: List[char]) -> String
Create a new string from a list of characters.

```{code-block} roto
:notest:

String.from_chars(['h', 'e', 'l', 'l', 'o']) # -> "hello"
```
````

````{roto:function} lines(self: String) -> StringLines
Get a view of this string indexed by lines.
````

````{roto:function} repeat(self: String, n: u32) -> String
Repeat a string `n` times and join them.

```{code-block} roto
:notest:

"ha".repeat(6) # -> "hahahahahaha"
```
````

````{roto:function} replace(self: String, from: String, to: String) -> String
Replace all occurrences of `from` with `to`.

```{code-block} roto
:notest:

"In rust we trust".replace("rust", "roto") # -> "In roto we troto"
```
````

````{roto:function} split(self: String, separator: String) -> List[String]
Split a string by a separator.

```{code-block} roto
:notest:

"one, two, three".split(", ") # -> ["one", "two", "three"]
```
````

````{roto:function} starts_with(self: String, prefix: String) -> bool
Check whether a string starts with a given prefix.

```{code-block} roto
:notest:

"haystack".starts_with("hay")   # -> true
"haystack".starts_with("trees") # -> false
```
````

````{roto:function} to_lowercase(self: String) -> String
Create a new string with all characters converted to lowercase.

```{code-block} roto
:notest:

"LOUD".to_lowercase() # -> "loud"
```
````

````{roto:function} to_string(self: String) -> String
Convert this value into a `String`
````

````{roto:function} to_uppercase(self: String) -> String
Create a new string with all characters converted to uppercase.

```{code-block} roto
:notest:

"quiet".to_uppercase() # -> "QUIET"
```
````

