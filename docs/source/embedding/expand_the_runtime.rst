Expand the runtime
==================

By default, Roto's standard library is minimal. The idea behind that is that
each application should supply the functionality that they allow the scripts to
use.

The standard runtime doesn't even have a way to print to stdout, because
the host application may not want to allow the scripts to do that. But it is
possible to add some standard modules to the runtime. For example, there is a
method ``add_io_functions`` on ``Runtime`` to add the ``print`` function.

.. code-block:: rust

    use roto::Runtime;
    let mut runtime = Runtime::new();
    runtime.add_io_functions();

However, Roto truly starts shining when you add your own functionality to it
that makes your application scriptable.

Add functions
-------------

Even though Roto provides a standard `print` function, you might not want
to expose that. For instance, you might want to let scripts print to a file
instead. We can do that quite easily with the `roto_function` macro. You can
use this macro as an attribute macro on a function and pass it the runtime you
want to add the function to. This function will then become available in the
scripts you compile.

.. code-block:: rust

    use std::fs::OpenOptions;
    use std::io::Write;
    use roto::{Runtime, roto_function};
    let mut runtime = Runtime::new();

    // Note: we use Arc<str> because that's the Rust type corresponding to a
    // Roto string.
    #[roto_function(runtime)]
    fn print(s: Arc<str>) {
        let mut file = OpenOptions::new()
            .create(true)
            .write(true)
            .append(true)
            .open("log.txt")
            .unwrap();

        let _ = writeln!(file, "{s}");
    }

Scripts compiled with this runtime can then use this custom ``print`` function.
The registered functions can have any any name you want, as long as its a valid
Roto identifier.

Here is another example which returns a value:

.. code-block:: rust

    #[roto_function(runtime)]
    fn factorial(n: u32) -> u32 {
        (1..=n).fold(1, |a, i| a * i)
    }

.. note::
    Note that it is only possible to register functions using types you've already
    registered. You should probably register all your types before registering
    functions.

You can also give the function a custom name by passing that to the macro.

.. code-block:: rust

    // This function will be available as "factorial", not "my_factorial_function"
    #[roto_function(runtime, factorial)]
    fn my_factorial_function(n: u32) -> u32 {
        (1..=n).fold(1, |a, i| a * i)
    }

Add types
---------

We can also add our own types to Roto. As an example, we'll add a ``Range``
type. All registered types must implement ``Clone``.

.. code-block:: rust

    #[derive(Clone, Copy, Debug)]
    struct Range {
        low: i64,
        high: i64,
    }

    // Or register_clone_type if the type doesn't implement Copy.
    runtime.register_copy_type::<Range>("A range of i64").unwrap();

The argument to that method is the docstring for this type. We can now pass this
type to Roto and return it from Roto:

.. code-block:: roto

    fn passthrough(x: Range) -> Range {
        x
    }

Not very useful yet, of course, but let's see it in action anyway:

.. code-block:: rust

    use roto::Val;

    let mut pkg = runtime.compile("script.roto").unwrap();
    let f = pkg
        .get_function::<_, fn(Val<Range>) -> Val<Range>>("passthrough")
        .unwrap();

    let res = f.call(&mut (), Val(Range { low: 0, high: 99 }));
    println!("{res:?}")

Note that every custom type has to be wrapped in ``Val`` when it's passed to
Roto, but otherwise it works exactly like before.

There are 4 methods you can choose from to register a type:

- ``Runtime::register_copy_type``
- ``Runtime::register_clone_type``
- ``Runtime::register_copy_type_with_name``
- ``Runtime::register_clone_type_with_name``

The first two will attempt to guess the name of the type from the type name in
Rust. If you want a custom name, you can use one of the bottom two methods. If
your type implements ``Copy`` you should use `register_copy_type`, because that
will allow Roto to generate slightly more performant code.

Add methods
-----------

To make the ``Range`` type we registered previously actually useful, we can
expose methods on it to Roto.

.. code-block:: rust

    use roto::roto_method;

    #[roto_method(runtime, Range)]
    fn contains(range: Val<Range>, x: i64) -> bool {
        range.low <= x && x < range.high
    }

    let mut pkg = runtime.compile("script.roto").unwrap();
    let f = pkg
        .get_function::<_, fn(Val<Range>, x: i64) -> bool>("in_range")
        .unwrap();

    let range = Range { low: 0, high: 99 };
    let res = f.call(&mut (), Val(range), 50);
    println!("{res:?}");

And then in Roto:

.. code-block:: roto

    fn in_range(r: Range, x: i64) -> bool {
        r.contains(x)
    }

Related to methods, there are static methods. These are methods that are called
without an instance of the type.

.. code-block:: rust

    use roto::roto_static_method;

    #[roto_static_method(runtime, Range)]
    fn new(low: i64, high: i64) -> Val<Range> {
        Val(Range { low, high })
    }

Which can be used in Roto like this:

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

    runtime.register_constant(
        "ONE_HUNDRED",
        "A range from 0 to 100",
        Val(Range { low: 0, high: 100 }),
    ).unwrap();

The name ``ONE_HUNDRED`` will then be available in Roto scripts.

.. _add-context:

Add context
-----------

In the previous section, we added constants to the ``Runtime``, but sometimes
constants are too restrictive. One such case is when we have a value that we
want to keep constant *throughout a single invocation* of a function. Or, to
phrase it another way, we might want to pass in some implicit arguments that
the script has access to.

Adding context is a bit more difficult because we need a single way to pass
all those implicit arguments. So, instead of registering each context variable
one by one, you have to create a context type by deriving the ``Context``
trait. You can then register that type as the context you want to use.

.. note::
    You can only register one context type per runtime.

Imagine that we same some script that operates on the data of some user. We
might then expose the name of that user to all scripts implicitly. We would
then create and register the following type.

.. code-block:: rust

    use roto::Context;

    #[derive(Context)]
    struct Ctx {
        pub first_name: Arc<str>,
        pub last_name: Arc<str>,
    }

    runtime.register_context_type::<Ctx>().unwrap();

    let mut pkg = runtime.compile("script.roto").unwrap();

    //                         We need to use the correct context type here
    //                         |
    //                         v
    let f = pkg.get_function::<Ctx, fn() -> Arc<str>>("greeting").unwrap();

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
