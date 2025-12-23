Expand the runtime
==================

By default, Roto's standard library is minimal. The idea behind that is that
each application should supply the functionality that they allow the scripts to
use.

The standard runtime doesn't even have a way to print to ``stdout``, because
the host application may not want to allow the scripts to do that. But it is
possible to add some standard modules to the runtime. For example, there is a
method ``add_io_functions`` on ``Runtime`` to add the ``print`` function.

.. code-block:: rust

    use roto::Runtime;
    let mut rt = Runtime::new();
    rt.add_io_functions();

However, Roto truly starts shining when you add your own functionality, so that your application becomes scriptable.

We support this via ``Runtime::from_lib`` and ``Runtime::add`` in combination
with the ``library!`` macro. You construct a library with that macro and then
add it to your runtime by calling one of those methods. ``Runtime::from_lib``
is simply a convenience method for creating a new runtime and adding the library
directly.

Add functions
-------------

Even though Roto provides a standard `print` function, you might not want
to expose that. For instance, you might want to let scripts print to a file
instead. We can do that quite easily with the `roto_function` macro. You can
use this macro as an attribute macro on a function and pass it the runtime you
want to add the function to. This function will then become available in the
scripts you compile.

.. note::

    We use ``Arc<str>`` because that's the Rust type corresponding to Roto's
    ``String`` type. You always have to use the Rust names of types when
    registering functions. To learn how the types correspond to each other, see
    :ref:`rust_interop`.

.. code-block:: rust

    use std::sync::Arc;
    use std::fs::OpenOptions;
    use std::io::Write;
    use roto::{Runtime, roto_function};

    let lib = library!{
        /// Print to the log file.
        fn print(s: Arc<str>) {
            let mut file = OpenOptions::new()
                .create(true)
                .write(true)
                .append(true)
                .open("log.txt")
                .unwrap();

            let _ = writeln!(file, "{s}");
        }
    };

    let rt = Runtime::from_lib(lib).unwrap();

Scripts compiled with this runtime can then use this custom ``print`` function.
The registered functions can have any name you want, as long as it's a valid
Roto identifier.

Here is another example which returns a value:

.. code-block:: rust

    let lib = library! {
        fn factorial(n: u32) -> u32 {
            (1..=n).fold(1, |a, i| a * i)
        }
    };

Add types
---------

We can also add our own types to Roto. As an example, we'll add a ``Range``
type. All registered types must implement ``Clone``. Every type you register
must be wrapped in `Val<T>`, which tells Roto that it is a custom type and
not built in.

.. code-block:: rust

    use roto::{library, Runtime, Val};

    #[derive(Clone, Copy, Debug)]
    struct Range {
        low: i64,
        high: i64,
    }

    let lib = library! {
        /// A range of i64 numbers
        #[copy] type Range = Val<Range>;
    };

    let rt = Runtime::from_lib(lib).unwrap();

The argument to that method is the docstring for this type. We can now pass this
type to Roto and return it from Roto:

.. code-block:: roto

    fn passthrough(x: Range) -> Range {
        x
    }

Not very useful yet, of course, but let's see it in action anyway:

.. code-block:: rust

    use roto::Val;

    let mut pkg = rt.compile("script.roto").unwrap();
    let f = pkg
        .get_function::<fn(Val<Range>) -> Val<Range>>("passthrough")
        .unwrap();

    let res = f.call(Val(Range { low: 0, high: 99 }));
    println!("{res:?}")

Note that every custom type has to be wrapped in ``Val`` when it's passed to
Roto, but otherwise it works exactly like before.

The ``#[copy]`` attribute above specifies that the Rust type implements
``Copy``. If the type does not implement ``Copy``, you can instead annotate the
declaration with ``#[clone]``. However, you should prefer ``#[copy]`` to allow
Roto to generate code that performs slightly better.

Add methods
-----------

To make the ``Range`` type we registered previously actually useful, we can
expose methods on it to Roto.

.. code-block:: rust

    let lib = library! {
        impl Val<Range> {
            fn contains(self, x: i64) -> bool {
                range.low <= x && x < range.high
            }
        }
    };

    let rt = Runtime::from_lib(lib);

    let mut pkg = rt.compile("script.roto").unwrap();
    let f = pkg
        .get_function::<_, fn(Val<Range>, x: i64) -> bool>("in_range")
        .unwrap();

    let range = Range { low: 0, high: 99 };
    let res = f.call(Val(range), 50);
    println!("{res:?}");

And then in Roto:

.. code-block:: roto

    fn in_range(r: Range, x: i64) -> bool {
        r.contains(x)
    }

The first argument of a function in an ``impl`` block does not need to be of
the same type as the one specified by the ``impl`` block. If that is the case,
this function can only be called with the full path and not as a method. In the
example below ``new`` is such a method.

.. code-block:: rust

    let lib = library! {
        impl Val<Range> {
            fn new(low: i64, high: i64) -> Self {
                Val(Range { low, high })
            }

            fn contains(range: Val<Range>, x: i64) -> bool {
                range.low <= x && x < range.high
            }
        }
    };

    let rt = Runtime::from_lib(lib).unwrap();

The registered ``new`` function can be used in Roto like this:

.. code-block:: roto

    let range = Range.new(0, 99);
    range.contains(50)

Add constants
-------------

Finally, we can register constants into the runtime. Like functions, we can
only add constants of types we've already registered. Along with the constant
we have to provide a docstring. This docstring will show up in the
documentation generated for this runtime.

.. code-block:: rust

    let lib = library! {
        /// A range from 0 to 100
        const ONE_HUNDRED: Val<Range> = Val(Range { low: 0, high: 100 });
    };

    let rt = Runtime::from_lib(lib).unwrap();

The name ``ONE_HUNDRED`` will then be available in Roto scripts.

.. _add-context:

Add context
-----------

In the previous section, we added constants to the ``Runtime``, but sometimes
constants are too restrictive. One such case is when we have a value that we
want to keep constant *throughout a single invocation* of a function. Or, to
phrase it another way, we might want to pass in some implicit arguments that
the script has access to.

Adding context is a bit more difficult, because we need a single way to pass
all those implicit arguments. So, instead of registering each context variable
one by one, you have to create a context type by deriving the ``Context``
trait. You can then register that type as the context you want to use.

.. note::
    You can only register one context type per runtime.

Imagine that we save some script that operates on the data of some user. We
might then expose the name of that user to all scripts implicitly. We would
then create and register the following type.

.. code-block:: rust

    use std::sync::Arc;
    use roto::Context;

    #[derive(Context)]
    struct Ctx {
        pub first_name: Arc<str>,
        pub last_name: Arc<str>,
    }

    let rt = rt.with_context_type::<Ctx>().unwrap();

    let mut pkg = rt.compile("script.roto").unwrap();
    let f = pkg.get_function::<fn() -> Arc<str>>("greeting").unwrap();

    let mut ctx = Ctx {
        first_name: "John".into(),
        last_name: "Doe".into(),
    };
    let greeting = f.call(&mut ctx);
    println!("{greeting}");

All the fields of ``Ctx`` have to be public, to acknowledge the fact that
they are exposed to Roto. The first argument of ``f.call`` is the context we
give to this invocation. The script can then use the names of the fields of
``Ctx`` as if they were constants.

.. code-block:: roto

    fn greeting() -> String {
        "Hello, " + first_name + " " + last_name + "!"
    }

Other use-cases of context are log files, unique ids per invocation or just to
provide easy access to some common data.

Add modules
-----------

Everything registered so far has been added to the global module. With a large
runtime, that quickly becomes unwieldy. We can fix this by using Roto's module
system. To do so, simply wrap the items you want to register in a Rust-like
`mod`.

You can nest `mod` statements to create complex structures.

.. code-block:: rust

    let lib = library! {
        mod math {
            /// Compute the sine of `x`
            fn sin(x: f64) -> f64 {
                x.sin()
            }

            /// Compute the cosine of `x`
            fn cos(x: f64) -> f64 {
                x.cos()
            }
        }
    };

    let rt = Runtime::from_lib(lib).unwrap();

The Rust code above registers the `sin` and `cos` functions in the `math` module, so
that's how we can then access it from Roto:

.. code-block:: roto

    # import the cos function so we can use it directly
    import math.cos;

    fn foo(x: f64) -> f64 {
        # we haven't imported sin so we have to access it via `math`
        math.sin(x)
    }

    fn foo(x: f64) -> f64 {
        cos(x)
    }

Composing libraries
-------------------

We are not limited to using just a single `library!` call, but we can create
multiple libraries and compose them using the `include!` macro inside a
`library!`. Doing so includes a library directly in another at the point where
the `include!` macro is used.

.. warning::

    Take care not to include items twice at different points in the hierarchy to
    avoid confusion.

.. code-block:: rust

    let foo = library! {
        #[clone] type Foo = Val<SomeRustType>;
    };

    let bar = library! {
        fn bar() -> u32 {
            5
        }
    };

    let baz = library! {
        // include the contents of foo directly
        include!(foo)

        // include the contents of bar inside the functions module
        mod functions {
            include!(bar)
        }
    };

See also
--------

For more information, see the documentation for the `library!
<https://docs.rs/roto/latest/roto/macro.library.html>`__ macro.
