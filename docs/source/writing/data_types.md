# Data Types

In [](writing-basics), we covered some of Roto's primitive types, but those
can only get you so far; you can construct your own types too!

## Anonymous Records

The simplest way to group some values is with an *anonymous record*. Create one using curly braces `{}` to wrap a list of the fields and their respective types. Create a value of that same type by using the same braces and assigning a value for every field. This is a convenient way to return multiple values from a function.

In this example, the function accepts an anonymous record containing two `f64` fields, `x` and `y`, and returns a new record with those fields doubled. The body of the function takes the values from the `a` parameter and multiplies them by 2 to create the new record that matches the "shape" of the declared return type.

```roto
fn double_both(a: {x: f64, y: f64}) -> {x: f64, y: f64} {
    {
        x: 2.0 * a.x,
        y: 2.0 * a.y,
    }
}

fn main() {
    let one = { x: 10.0, y: 20.0 };
    let two = double_both(one);
    print(f"two.x = {two.x}");
    print(f"two.y = {two.y}");
}
```

:::{testoutput}
two.x = 20
two.y = 40
:::

:::{seealso}
[`anonymous records`](lang_anonymous_records) in the language reference
:::

## Record Types

Of course, writing the same anonymous record types gets tedious, so you
can also make *named* record types with the [`record`](lang_named_records) keyword.

Here is the same snippet as before but using a named record type instead of an anonymous one:

```roto
record Vec2 {
    x: f64,
    y: f64,
}


fn double_both(a: Vec2) -> Vec2 {
    Vec2 {
        x: 2.0 * a.x,
        y: 2.0 * a.y,
    }
}

fn main() {
    let one = { x: 10.0, y: 20.0 };
    let two = double_both(one);
    print(f"two.x = {two.x}");
    print(f"two.y = {two.y}");
}
```

:::{hint}
A `record` in Roto is similar to a `struct` or `class` in other languages.
:::

:::{seealso}
[`named_records`](lang_named_records) in the language reference
:::

## Enum Types

Records are great when you want to create a type that is a combination of
multiple fields, but often, you need something that contains one of several
options. For example, text might be left, center or right aligned. We can model
that in Roto with [`enum`](lang_enum_type) types.

We start by declaring the type `Alignment` with three constructors: `Left`,
`Center`, `Right`. If we now write `Alignment.Left`, for instance, we make
a value of that type representing left alignment. For any value of type
`Alignment`, we can [`match`](lang_match) on it to specify what
we should do in each case.

```roto
enum Alignment {
    Left,
    Center,
    Right,
}

fn align(x: String, size: u64, alignment: Alignment) -> String {
    let x_size = x.chars().len();
    if size <= x_size {
        return x;
    }

    let pad = size - x_size;
    let left = 0;
    let right = 0;
    match alignment {
        Left => {
            right = pad;
        }
        Right => {
            left = pad;
        }
        Center => {
            left = pad / 2;
            right = pad / 2 + pad % 2;
        }
    }

    let left_str = "_".repeat(left);
    let right_str = "_".repeat(right);
    f"{left_str}{x}{right_str}"
}

fn main() {
    print(align("Hey!", 10, Alignment.Left));
    print(align("Hey!", 10, Alignment.Right));
    print(align("Hey!", 10, Alignment.Center));
    print(align("Ho!", 10, Alignment.Left));
    print(align("Ho!", 10, Alignment.Right));
    print(align("Ho!", 10, Alignment.Center));
}
```

:::{testoutput}
Hey!______
______Hey!
___Hey!___
Ho!_______
_______Ho!
___Ho!____
:::

Enum types can also carry data in their constructors by specifying a list
of types enclosed in `()`. You can extract the values of these fields while
matching on the type. Below, we create a type `Range` that represents a range of
`i64` numbers, where you can either specify no bounds, a lower bound, an upper bound or
both. The `contains` function then checks whether a number is within that range.

```roto
enum Range {
    Full,
    From(i64),
    Until(i64),
    Between(i64, i64),
}

fn contains(range: Range, x: i64) -> bool {
    match range {
        Full => true,
        From(a) => x >= a,
        Until(a) => x <= a,
        Between(a, b) => x >= a && x <= b,
    }
} 

fn main() {
    let a = contains(Range.Full, 5);
    let b = contains(Range.From(6), 5);
    let c = contains(Range.Until(7), 5);
    let d = contains(Range.Between(6, 10), 5);
    print(f"a = {a}, b = {b}, c = {c}, d = {d}");
}
```

:::{testoutput}
a = true, b = false, c = true, d = false
:::

:::{seealso}
[`enum`](lang_enum_type) and [`match`](lang_match) in the language reference
:::
