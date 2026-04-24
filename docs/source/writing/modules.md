# Modules & Imports

As your scripts get larger, it's a good idea to split them into multiple files.
Roto helps you manage them via _modules_.

To enable support for multiple files, pass a folder instead of a file when you run Roto:

```console
$ roto run path/to/directory
```

In that directory, create a file called `pkg.roto`. This file is the
root of your module tree (or your _package_, hence the name). Put your
`main` function in this file. Any files ending in `.roto` next to it will become
submodules of `pkg`.

If you add a directory containing a file called `lib.pkg`, that will also become
a submodule and any Roto files within it will be considered submodules of that module.
You can keep going like this as far as you want.

@todo the example doesn't contain a `lib.pkg` file, but it should to show how that works.

Say we have a folder `my_roto_project` containing the following files:

```text
my_roto_project
├── bar
│   ├── baz.roto
│   └── lib.roto
├── foo.roto
└── pkg.roto
```

That will create the following module tree:

```text
pkg
├── foo
└── bar
    └── baz
```

This module tree is constructed automatically; you don't need any special code
to use items from another module. Say, for instance, that `bar` contains a
function called `double`. Then in `pkg.roto`, we can refer to that function via
`bar` because `bar` is a submodule of `pkg`.

{class="test-ignore"}
```roto
fn main() {
    bar.double(2);
}
```

In any file we can refer to this function by its absolute name:
`pkg.bar.double`. This works because Roto treats any path starting with `pkg` as
an absolute path.

You can explicitly [`import`](lang_imports) individual items from modules
into your current file's namespace. If we `import bar.double`, then we can use
`double` to refer to `bar.double`.

@todo I'm not sure if namespace is the right word here, but it is the same concept as in other languages.

{class="test-ignore"}
```roto
import bar.double;

fn main() {
    double(2);
}
```

Multiple [`import`](lang_imports) statements can be combined into one if
you need to import multiple items from the same module. While this can make
your code more concise, it doesn't play quite so well with version control
differences so you may want to avoid it in larger projects.

{class="test-ignore"}
```roto
import bar.{double, triple};
# is identical to
import bar.double;
import bar.triple;
```

:::{seealso}
[](lang_modules) in the language reference

[](lang_imports) in the language reference
:::
