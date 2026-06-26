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


````{roto:method} String.append(self: String, other: String) -> String
Append a string to another, creating a new string.

```roto
let a = "hello".append(" ").append("world"); // -> "hello world"
```
````

````{roto:method} String.bytes(self: String) -> StringBytes
Get a view of this string indexed by bytes.
````

````{roto:method} String.chars(self: String) -> StringChars
Get a view of this string indexed by chars.
````

````{roto:method} String.contains(self: String, needle: String) -> bool
Check whether a string contains another string.

```roto
let a = "haystack".contains("hay");  // -> true
let b = "haystack".contains("corn"); // -> false
```
````

````{roto:method} String.ends_with(self: String, suffix: String) -> bool
Check whether a string ends with a given suffix.

```roto
let a = "haystack".ends_with("stack"); // -> true
let b = "haystack".ends_with("black"); // -> false
```
````

````{roto:method} String.eq(self: String, other: String) -> bool
Check for string equality.
````

````{roto:method} String.from_chars(chars: List[char]) -> String
Create a new string from a list of characters.

```roto
let a = String.from_chars(['h', 'e', 'l', 'l', 'o']); // -> "hello"
```
````

````{roto:method} String.lines(self: String) -> StringLines
Get a view of this string indexed by lines.
````

````{roto:method} String.repeat(self: String, n: u64) -> String
Repeat a string `n` times and join them.

```roto
let a = "ha".repeat(6); // -> "hahahahahaha"
```
````

````{roto:method} String.replace(self: String, from: String, to: String) -> String
Replace all occurrences of `from` with `to`.

```roto
let a = "In rust we trust".replace("rust", "roto"); // -> "In roto we troto"
```
````

````{roto:method} String.rsplitn(self: String, n: u64, separator: String) -> List[String]
Splits this string at `separator` at most `n` times starting
from the end.

If there are more than `n - 1` separators, the last list element
will contain the remaining prefix of the string.

```roto
let a = "Rust!Roto!String".rsplitn(2, "!");
// -> ["String", "Rust!Roto"]
```
````

````{roto:method} String.split(self: String, separator: String) -> List[String]
Split a string by a separator.

```roto
let a = "one, two, three".split(", "); // -> ["one", "two", "three"]
```
````

````{roto:method} String.splitn(self: String, n: u64, separator: String) -> List[String]
Splits this string at `separator` at most `n` times.

If there are more than `n - 1` separators, the last list element
will contain the rest of the string.

```roto
let a = "Rust!Roto!String".splitn(2, "!");
// -> ["Rust", "Roto!String"]
```
````

````{roto:method} String.starts_with(self: String, prefix: String) -> bool
Check whether a string starts with a given prefix.

```roto
let a = "haystack".starts_with("hay");   // -> true
let b = "haystack".starts_with("trees"); // -> false
```
````

````{roto:method} String.strip_prefix(self: String, prefix: String) -> Option[String]
Create a new string by removing a given prefix.

Returns `None` if the string does not contain the prefix.

```roto
let a = "RustRoto!".strip_prefix("Rust"); // -> "Roto!"
```
````

````{roto:method} String.strip_suffix(self: String, suffix: String) -> Option[String]
Create a new string by removing a given suffix.

Returns `None` if the string does not contain the suffix.

```roto
let a = "Roto!Rust".strip_suffix("Rust"); // -> "Roto!"
```
````

````{roto:method} String.to_lowercase(self: String) -> String
Create a new string with all characters converted to lowercase.

```roto
let a = "LOUD".to_lowercase(); // -> "loud"
```
````

````{roto:method} String.to_string(self: String) -> String
Convert this value into a `String`
````

````{roto:method} String.to_uppercase(self: String) -> String
Create a new string with all characters converted to uppercase.

```roto
let a = "quiet".to_uppercase(); // -> "QUIET"
```
````

````{roto:method} String.trim(self: String) -> String
Create a new string by removing leading and trailing
whitespace.

```roto
let a = "  Roto!  ".trim(); // -> "Roto!"
```
````

````{roto:method} String.trim_end(self: String) -> String
Create a new string by removing trailing whitespace.

```roto
let a = "  Roto!  ".trim_end(); // -> "  Roto!"
```
````

````{roto:method} String.trim_start(self: String) -> String
Create a new string by removing leading whitespace.

```roto
let a = "  Roto!  ".trim_start(); // -> "Roto!  "
```
````

