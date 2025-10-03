Call a Roto function
====================

Now we can start loading and compiling a Roto script. The first thing we need
is a ``Runtime``, which is the value that defines all the types and functions
that are available to Roto. We'll start out with the most basic ``Runtime`` by
calling ``Runtime::new`` (step 1).

We load and compile a script by calling the ``Runtime::compile`` method (step 2). A
compiled script can contain many different functions, so we pick one that we
want to extract by calling the ``get_function`` method, with the function type we
expect and the name of the function (step 3).

Finally, we call the extracted Roto function with the ``call`` method (step 4). We
pass it a context of ``&mut ()`` (explained in :ref:`add-context`) and the
parameter ``4``.

.. code:: rust

    use roto::Runtime;

    fn main() {
        // Step 1: Create a runtime
        let rt = Runtime::new();

        // Step 2: Compile the script and check for type errors
        let result = rt.compile("script.roto");
        let mut pkg = match result {
            Ok(pkg) => pkg,
            Err(err) => {
                panic!("{err}");
            }
        }

        // Step 3: Extract the function
        let func = pkg
            .get_function::<(), fn(i32) -> i32>("times_two")
            .unwrap();

        // Step 4: Call the function
        let result = func.call(&mut (), 4);
        println!("times_two(4) = {result}");
    }

Now we can create roto script that can be loaded by this Rust application. In
the Rust application, we ask for a function called ``times_two`` with the type
``fn(i32) -> i32``, so that's what we have to write in Roto, too. Let's make it
a function that doubles its input.

.. code-block:: roto

    fn times_two(x: i32) -> i32 {
        2 * x
    }

Save that as ``script.roto`` and run the application. If that gives you the
following output then everything works as expected!

.. code-block:: console

    $ cargo run
    times_two(4) = 8

You can write functions with multiple arguments and other types as well, as long
as the types in Rust match the types in Roto.

In the next chapter, we'll expose Rust types and functions to our Roto script.
