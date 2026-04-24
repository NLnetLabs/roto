(writing-basics)=
# Basics

## Printing

The first thing you should know about Roto is how to make it print something.
That is done with the {roto:ref}`print` function, which takes a single
{roto:ref}`String` as input. [Strings](lang_strings) are delimited by double
quotes (`"`).

```roto
fn main() {
    print("Hello, World!");
}
```

```{testoutput}
Hello, World!
```

Variables are created with [`let`](lang_locals). In the example below, we create
a variable called `first_name` that holds a string. A variable's type is usually
inferred (in this example, the fact that we are assigning a string literal to
the variable implies that it is a string type), but you can be explicit about
it by adding a _type annotation_. Roto's type checker will then check that the
variable has that type and produce an error if you try to use it in a way that
is not compatible with the string type. Because it's a string, we can join it to
other strings using the `+` operator.

```roto
fn main() {
    // Create a variable with the inferred type String
    let first_name = "John";
    print("Hello, " + first_name);

    // Create a variable with a type annotation
    let last_name: String = "Doe";
    print("Bye, " + first_name + " " + last_name + "!");
}
```

:::{testoutput}
Hello, John
Bye, John Doe!
:::

A more convenient way to build complex strings is with string
formatting. A string prefixed with `f` will act as a [format string (or
f-string)](lang_string_formatting) where code in curly braces (`{}`) will be
evaluated as a Roto expression and inserted at that location in the string.
Usefully, this includes the ability to perform a conversion from the variable's
type to a string (assuming the type knows how to do that), so we don't have to
worry about that when using f-strings.

```roto
fn main() {
    let first_name = "John";
    let last_name = "Doe";
    print(f"Hello, {first_name} {last_name}!");
}
```

:::{testoutput}
Hello, John Doe!
:::

We are not limited to using names of variables in f-strings, we can also
use more complex expressions. For instance, we can convert the name to
uppercase before printing it.

```roto
fn main() {
    let name = "John";
    print(f"Hello, {name.to_uppercase()}");
}
```

:::{testoutput}
Hello, JOHN
:::

:::{seealso}

[Strings](lang_strings) in the language reference.

The {roto:ref}`String` type.

The {roto:ref}`print` function.
:::

## Simple Calculations

Of course, Roto doesn't just have strings. The next thing we should introduce
are Roto's numeric types. The simplest numeric types in Roto are integers. There
are two main kinds of integers: signed and unsigned. Since integers are used
very often, we use short names for them. Types of signed integers are prefixed
with `i` followed by their size in bits: {roto:ref}`i8`, {roto:ref}`i16`,
{roto:ref}`i32` and {roto:ref}`i64`. Unsigned integers are the same but with `u`
instead of `i`. Signed integers can express negative values, but have half the
size of unsigned integers. So for example, `i8` can express values from -128 to
127, while `u8` can express values from 0 to 255.

The integers support many operators that you might expect, such as `+`
for addition, `-` for subtraction, `*` for multiplication, `/` for
division and `%` for remainder. These operators follow the conventional
rules for precedence.

While we can't print integers directly, we can use them in f-strings.

```roto
fn main() {
    // We can specify the type to get a specific integer type
    let a: u8 = 2 + 1;
    print(f"2 + 1 = {a}");

    // If we don't specify the type we get i32
    let b = -10 + 3;
    print(f"-10 + 3 = {b}");

    let c = 2 * 3;
    print(f"2 * 3 = {c}");

    let d = 20 / 5;
    print(f"20 / 5 = {d}");

    let e = 23 % 5;
    print(f"23 % 5 = {e}");

    // Regular order of operations applies
    let f = 1 + 4 * 5;
    print(f"1 + 4 * 5 = {f}");
}
```

:::{testoutput}
2 + 1 = 3
-10 + 3 = -7
2 * 3 = 6
20 / 5 = 4
23 % 5 = 3
1 + 4 * 5 = 21
:::

Note that due to Roto's static typing, we aren't allowed to perform
operations on different types.

{class="test-error"}
```roto
fn main() {
    let a: u8 = 5;
    let b: u32 = 10;

    let c = a + b; // error!
}
```

Floating point numbers can represent fractional numbers. There are two
floating point types: {roto:ref}`f32` and {roto:ref}`f64` which are 32- and
64-bit, respectively. They support all the same operators as integers, except
for `%`.

To disambiguate floating point numbers from integers, especially when inferring
types, floating point literals must include a period `.`. So `10` is always an
integer, but `10.` and `10.0` are always floating point numbers.

```roto
fn main() {
    // We can specify the type to get a specific integer type
    let a: f32 = 2.0 + 1.0; // -> 3.0
    print(f"2.0 + 1.0 = {a}");

    // We always have to write the period for floating point numbers
    let b: f32 = 2. + 1.; // -> 3.0
    print(f"2. + 1. = {b}");

    // If we don't specify the type we get f64
    let c = -12.3 + 4.5; // -> -7.8
    print(f"-12.3 + 4.5 = {c}");

    let d = 2.0 * 3.5; // -> 7.0
    print(f"2.0 * 3.5 = {d}");

    let e = 20.0 / 5.0; // -> 4.0
    print(f"20.0 / 5.0 = {e}");

    // Regular order of operations applies
    let f = 1.0 + 4.0 * 5.0; // -> 21.0
    print(f"1.0 + 4.0 * 5.0 = {f}");
}
```

:::{testoutput}
2.0 + 1.0 = 3
2. + 1. = 3
-12.3 + 4.5 = -7.800000000000001
2.0 * 3.5 = 7
20.0 / 5.0 = 4
1.0 + 4.0 * 5.0 = 21
:::

:::{seealso}

{ref}`lang_integers` in the language reference.

{ref}`lang_floats` in the language reference.

{ref}`lang_arithmetic` in the language reference.

{roto:ref}`u8`, {roto:ref}`u16`, {roto:ref}`u32`, {roto:ref}`u64`

{roto:ref}`i8`, {roto:ref}`i16`, {roto:ref}`i32`, {roto:ref}`i64`

{roto:ref}`f32`, {roto:ref}`f64`
:::

## Booleans and Comparisons

Booleans in Roto are represented by the {roto:ref}`bool` type. It has two
possible values: `true` and `false`. Like any built-in type, booleans can be
used directly in an [f-string](lang_string_formatting) to be printed. A boolean
can be negated with the `!` operator.

```roto
fn main() {
    let a = !false;
    print(f"{a}");
}
```

:::{testoutput}
true
:::

The other important operators for boolean values are the `&&` and `||`
operators that represent the "logical and" and "logical or"
operations, respectively.

Booleans can also be created as the result of comparisons. In Roto, you can use
the equality operator `==` on all types (note that's a _double_ equals sign; the
single equals sign is used for assignment, as we have already seen).

```roto
fn main() {
    let x = 5 == 5;
    let y = "hello" == "hello";
    let z = true == true;
    print(f"{x && y && z}");
}
```

:::{testoutput}
true
:::

:::{warning}
Take care when using `==` and `!=` on floats, because they check for exact
equality, but floating point numbers are often only _approximately_ equal due
to the way they are represented in memory, so two floats that are mathematically
equal might not be exactly equal in practice. A `nan` value ("not a number") is
also not equal to itself.
:::

The opposite of the `==` is the `!=` operator, which returns `true` if
the operands are *not* equal.

Integers and floats also support the comparison operators for "less
than" (`<`), "less than or equal" (`<=`), "greater than" (`>`) and
"greater than or equal" (`>=`).

```roto
fn main() {
    let x = 5 > 3;
    print(f"{x}");
}
```

:::{testoutput}
true
:::

:::{seealso}

{ref}`lang_booleans` in the language reference

{ref}`lang_comparison` in the language reference

{ref}`lang_logical` in the language reference

{roto:ref}`bool`
:::

## Lists

[Lists](lang_lists) are the most common (and currently also the only) collection
type in Roto. Create a list by wrapping the set of values in `[]`, and
separating the elements using commas. A List's type is {roto:ref}`List[T]` where
`T` is the type of the elements.

```roto
fn main() {
    let x: List[i32] = [1, 2, 4];
    let len = x.len();
    let contains_2 = x.contains(2);
    let contains_3 = x.contains(3);
    print(f"length of x: {len}");
    print(f"x contains 2: {contains_2}");
    print(f"x contains 3: {contains_3}");
}
```

:::{testoutput}
length of x: 3
x contains 2: true
x contains 3: false
:::

Unlike in some other scripting languages, all the elements of a list must be
of the same type. You'll get a type checking error if you try to make a list
containing an integer and a string for example:

{class="test-error"}
```roto
fn main() {
    let x = [1, "hello!"];
}
```

You can build larger lists by concatenating multiple lists using the `+`
operator.

```roto
fn main() {
    let x = [1, 2, 3];
    let y = [4, 5, 6];
    let z = x + y;

    let contains_2 = z.contains(2);
    let contains_5 = z.contains(5);
    print(f"z contains 2: {contains_2}");
    print(f"z contains 5: {contains_5}");
}
```

:::{testoutput}
z contains 2: true
z contains 5: true
:::
