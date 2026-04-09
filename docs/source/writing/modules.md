# Modules & Imports

As your scripts get larger, you probably want to split them over multiple files. Roto allows
that via modules.

To enable support for multiple files, you should pass a folder instead of a file to Roto.

```console
$ roto run path/to/directory
```

In that directory, you should put a file called `pkg.roto`. This is file is the
root of your module tree (or your _package_, hence the name). You should put your
`main` function in this file. Any files ending in `.roto` next to it will become
submodules of `pkg`.

If you add a directory containing a file called `lib.pkg`, that will also become
a submodule and any Roto files in there will be submodules of that module. You can
do this as far as you want.

Say we have a folder `my_roto_project` with the following files:

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

This module tree is constructed automatically and you don't need any code
to use items from another module. Say, for instance, that `bar` contains a
functions called `double`. Then in `pkg.roto`, we can refer to that function via
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

You can [`import`](lang_imports) items into your current file. If we import
`bar.double`, then we can use `double` to refer to `bar.double`.

{class="test-ignore"}
```roto
import bar.double;

fn main() {
    double(2);
}
```

Multiple [`import`](lang_imports) statements can be combined into one if you
need to import multiple items from the same module.

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
