# String
`````{roto:type} String
The string type.

A `String` literal is created by putting some text between double
quotes.

```roto
let a = "Hello!";
```

See [the language reference](#lang_strings) for more information.
Roto supports string formatting when a string literal is prefixed
with an `f`.
`````


````{roto:function} append(self: String, other: String) -> String
Append a string to another, creating a new string.

```roto
let a = "hello".append(" ").append("world"); // -> "hello world"
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

```roto
let a = "haystack".contains("hay");  // -> true
let b = "haystack".contains("corn"); // -> false
```
````

````{roto:function} ends_with(self: String, suffix: String) -> bool
Check whether a string ends with a given suffix.

```roto
let a = "haystack".ends_with("stack"); // -> true
let b = "haystack".ends_with("black"); // -> false
```
````

````{roto:function} eq(self: String, other: String) -> bool
Check for string equality.
````

````{roto:function} from_chars(chars: List[char]) -> String
Create a new string from a list of characters.

```roto
let a = String.from_chars(['h', 'e', 'l', 'l', 'o']); // -> "hello"
```
````

````{roto:function} lines(self: String) -> StringLines
Get a view of this string indexed by lines.
````

````{roto:function} repeat(self: String, n: u64) -> String
Repeat a string `n` times and join them.

```roto
let a = "ha".repeat(6); // -> "hahahahahaha"
```
````

````{roto:function} replace(self: String, from: String, to: String) -> String
Replace all occurrences of `from` with `to`.

```roto
let a = "In rust we trust".replace("rust", "roto"); // -> "In roto we troto"
```
````

````{roto:function} split(self: String, separator: String) -> List[String]
Split a string by a separator.

```roto
let a = "one, two, three".split(", "); // -> ["one", "two", "three"]
```
````

````{roto:function} starts_with(self: String, prefix: String) -> bool
Check whether a string starts with a given prefix.

```roto
let a = "haystack".starts_with("hay");   // -> true
let b = "haystack".starts_with("trees"); // -> false
```
````

````{roto:function} strip_prefix(self: String, prefix: String) -> Option[String]
Create a new string by removing a given prefix.

Returns `None` if the string does not contain the prefix.

```roto
let a = "RustRoto!".strip_prefix("Rust"); // -> "Roto!"
```
````

````{roto:function} strip_suffix(self: String, suffix: String) -> Option[String]
Create a new string by removing a given suffix.

Returns `None` if the string does not contain the suffix.

```roto
let a = "Roto!Rust".strip_suffix("Rust"); // -> "Roto!"
```
````

````{roto:function} to_lowercase(self: String) -> String
Create a new string with all characters converted to lowercase.

```roto
let a = "LOUD".to_lowercase(); // -> "loud"
```
````

````{roto:function} to_string(self: String) -> String
Convert this value into a `String`
````

````{roto:function} to_uppercase(self: String) -> String
Create a new string with all characters converted to uppercase.

```roto
let a = "quiet".to_uppercase(); // -> "QUIET"
```
````

````{roto:function} trim(self: String) -> String
Create a new string by removing leading and trailing
whitespace.

```roto
let a = "  Roto!  ".trim(); // -> "Roto!"
```
````

````{roto:function} trim_end(self: String) -> String
Create a new string by removing trailing whitespace.

```roto
let a = "  Roto!  ".trim_end(); // -> "  Roto!"
```
````

````{roto:function} trim_start(self: String) -> String
Create a new string by removing leading whitespace.

```roto
let a = "  Roto!  ".trim_start(); // -> "Roto!  "
```
````

