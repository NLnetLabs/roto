Expand the runtime
==================

By default, Roto's standard library is minimal. The idea behind that is that
each application should supply the functionality that they allow the scripts to
use.

The standard runtime doesn't even have a way to print to stdout, because
the host application doesn't want to allow the scripts to do that. But it is
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

However, you might not want the standard implementation of `print`. For
instance, you might want to let scripts print to a file instead. We can do that
quite easily.

.. code-block:: rust

    use roto::{Runtime, roto_function};
    let mut runtime = Runtime::new();

    // Note: we use Arc<str> because that's the Rust type corresponding to a
    // Roto string.
    #[roto_function(runtime)]
    fn print(s: Arc<str>) {
        let mut file = OpenOptions::new()
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

Add types
---------

We can also add our own types to Roto. As an example, we'll add a ``Range``
type. All types we add must implement ``Clone`` (or ``Copy`` for a bit more
performance).

.. code-block:: rust

    #[derive(Clone, Copy, Debug)]
    struct Range {
        low: i64,
        high: i64,
    }

    // Or register_clone_type if the type doesn't implement Copy.
    runtime.register_copy_type::<Range>("A range of i64").unwrap();

We can now pass this type to Roto and return it from Roto:

.. code-block:: roto

    function passthrough(x: Range) -> Range {
        x
    }

Not very useful yet, of course, but let's see it in action:

.. code-block:: rust

    use roto::Val;

    let compiled = runtime.read("script.roto").unwrap();
    let f = compiled
        .get_function::<_, fn(Val<Range>) -> Val<Range>>("passthrough")
        .unwrap();

    let res = f.call(&mut (), Val(Range { low: 0, high: 99 }));
    println!("{res:?}")

Note that every custom type has to be wrapped in ``Val`` when it's passed to
Roto, but otherwise it works exactly like before.

Add methods
-----------

To make the ``Range`` type we registered previously useful, we can expose
methods on it to Roto.

.. code-block:: rust

    #[roto_method(runtime, Range)]
    fn contains(range: Val<Range>, x: i64) -> bool {
        range.low <= x && x < range.high
    }

    let compiled = runtime.read("script.roto").unwrap();
    let f = compiled
        .get_function::<_, fn(Val<Range>, x: i64) -> Val<Range>>("in_range")
        .unwrap();

    let range = Range { low: 0, high: 99 };
    let res = f.call(&mut (), Val(range), 50);
    println!("{res:?}")

And then in Roto:

.. code-block:: roto

    function in_range(r: Range, x: i64) -> bool {
        r.contains(x)
    }

Related to methods, there are static methods. These are methods that are called
without an instance of the type.

.. code-block:: rust

    #[roto_static_method(runtime, Range)]
    fn new(low: i64, high: i64) -> Val<Range> {
        Range { low, high }
    }

Which can be used in Roto like this:

.. code-block:: roto

    let range = Range.new(0, 99);
    range.contains(50)

Add constants
-------------

TODO