# Functions

To organize and reuse our code, we can create functions. You already know what a
basic function looks like, because we've been using a `main` function as
entry point of the program.

Each function can take arguments, which require a name and a type separated by a
colon (`:`) and, of course, a function can also return a value. The return type
is indicated by `->` after the list of arguments. A function that does not have
an explicit return type returns the type `()`, which is the unit type.

The function body is a block that returns the last expression if there is no
semicolon at the end. You can also return early using the
[`return`](lang_functions) keyword.

```roto
fn double(x: u32) -> u32 {
    2 * x    # <- no semicolon!
}

fn triple(x: u32) -> u32 {
    return 3 * x;
}

fn main() {
    let doubled = double(14);
    print(f"double(14) = {doubled}");

    let tripled = triple(14);
    print(f"triple(14) = {tripled}");
}
```

:::{testoutput}
double(14) = 28
triple(14) = 42
:::

:::{hint}
The order in which functions are defined does not matter. You can pick any order
that you like!
:::

Recursion is also supported. For example, here is a recursive definition of the
factorial function. Note that the entire [`if`](lang_if_else) is the final
expression which is returned, so we do not need any [`return`](lang_functions)
statements.

```roto
fn factorial(n: u64) -> u64 {
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn main() {
    let res = factorial(5);
    print(f"factorial(5) = {res}");
}
```

:::{testoutput}
factorial(5) = 120
:::

:::{seealso}
[](lang_functions) in the language reference
:::
