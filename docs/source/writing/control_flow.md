# Control Flow

After introducing the basic types and operations, it's time we delve
into more complex programs with control flow.

## If-Else

An [`if-else`](lang_if_else) expression allows you to branch based on a boolean value.
In contrast with some other languages, Roto does not require parentheses
around the condition, but it does require curly braces for the body. You
can use the [`if-else`](lang_if_else) expression as a statement. The
[`else`](lang_if_else) part is then optional.

```roto
fn main() {
    let x = 100;
    if x % 2 == 0 {
        print("x is even");
    } else {
        print("x is odd");
    }
}
```

:::{testoutput}
x is even
:::

An [`if-else`](lang_if_else) expression also evaluates to a value itself. That
value is determined by the last value in each of the arms, as long as there is
no semicolon at the end.

```roto
fn main() {
    let x = 100;
    let sign = if x > 0 {
        print("x is positive");
        1       // <- No semicolon!
    } else {
        print("x is negative");
        -1      // <- No semicolon!
    };
    print(f"{sign}");
}
```

:::{testoutput}
x is positive
1
:::

It's possible to declare variables in the arms of [`if-else`](lang_if_else)
expressions, but they will only be available within that arm. The same goes
for any block of statements in Roto that is delimited by `{}`.

{class="test-error"}
```roto
fn main() {
    if true {
        let x = 10;
        print(f"{x}"); // This is fine!
    }
    print(f"{x}"); // This errors during type checking!
}
```

:::{seealso}
[](lang_if_else) in the language reference
:::

## While Loops

We can loop in Roto using a [`while`](lang_while) loop. As you might expect from
other languages, a [`while`](lang_while) loop takes a condition and then a block.
It will keep running the body until the condition evaluates to `false`.

```roto
// Euclidean algorithm for greatest common divisor
fn main() {
    let a_initial = 125;
    let b_initial = 50;

    let a = a_initial;
    let b = b_initial;
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }

    print(f"gcd({a_initial}, {b_initial}) = {a}")
}
```

:::{testoutput}
gcd(125, 50) = 25
:::

:::{seealso}
[](lang_while) in the language reference
:::

## For Loops

If you'd like to execute some code for every element in a list, you should use
a [`for`](lang_for) loop instead of a [`while`](lang_while) loop.

```roto
fn main() {
    for x in [10, 20, 30] {
        let squared = x * x;
        print(f"{x} squared is {squared}");
    }
}
```

:::{testoutput}
10 squared is 100
20 squared is 400
30 squared is 900
:::

:::{note}
A [`while`](lang_while) loop is currently the best way to iterate over a range
of numbers. There will be support for using [`for`](lang_for) loops with ranges
in the future. See [this community
post](https://community.nlnetlabs.nl/t/range-types-and-literals/74) for more
information.
:::

:::{seealso}
[](lang_for) in the language reference
:::
