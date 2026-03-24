Basics
======

This chapter will introduce Roto and guide you towards being able to write
your own scripts with it.

While you're learning the language, there are probably two pages you want to

The easiest way to follow along is to :ref:`install the standalone
compiler <installation>` and run the examples locally. You can
run a script with the following command:

.. code-block:: console

    $ roto run path-to-script.roto

When a Roto script is run, it is first compiled and any syntax or type checking
errors will be reported. Then the function called ``main`` will be called. The
minimal script you need is therefore the follow.

.. code-block:: roto

    fn main() {
        # your code goes here
    }

Every statement you write in Roto has to end with a semicolon. You can add
comments to your code with ``#``, which will make Roto ignore the rest of the
line.

Printing
--------

The first thing you should know about Roto is how to make it print something.
That is done with the ``print`` function, which takes a single ``String`` as
input. Strings are delimited by double quotes ``"``.

.. code-block:: roto

    fn main() {
        print("Hello, world!");
    }

Variables are created with ``let``. For example, we can create a variable
``name`` that holds a string, which we can then join with other strings with the
`+` operator.

.. code-block:: roto

    fn main() {
        let name = "John";
        print("Hello, " + name);
        # prints: "Hello, John"
        print("Bye, " + name + "!");
        # prints: "Bye, John!"
    }

A more convenient way to build complex strings is with string formatting. A
string prefixed with ``f`` will act as a format string (or f-string) where
text between ``{}`` will be evaluated as a Roto expression and inserted at that
location in the string.

.. code-block:: roto

    fn main() {
        let name = "John";
        print(f"Hello, {name}");
        # prints: "Hello, John"
        print(f"Bye, {name}!");
        # prints: "Bye, John!"
    }

We are not limited to using names of variables in f-strings, we can also use
more complex expressions. For instance, we can convert the name to uppercase
before printing it.

.. code-block:: roto

    fn main() {
        let name = "Jan";
        print(f"Hello, {name.to_uppercase()}");
        # prints "Hello, JOHN"
    }

You can find more string methods under the :roto:ref:`String` documentation.

Roto as a calculator
--------------------

Of course, Roto doesn't just have strings. The next thing we should introduce
are Roto's number types. The simplest number types in Roto are the integers.
There are two main kinds of integers: signed and unsigned. Since integers are
used very often, we use short names for them. Types of signed integers are
prefixed with ``i`` followed by their size in bits: :roto:ref:`i8`,
:roto:ref:`i16`, :roto:ref:`i32` and :roto:ref:`i64`. Unsigned integers are the
same but with ``u`` instead of ``i``.

The integers support many operators that you might expect, such as ``+`` for
addition, ``-`` for subtraction, ``*`` for multiplication, ``/`` for division
and ``%`` for remainder. These operators follow the conventional rules for
precedence.

.. code-block:: roto

    fn main() {
        # We can specify the type to get a specific integer type
        let a: u8 = 2 + 1; # -> 3
        print(f"2 + 1 = {a}");

        # If we don't specify the type we get i32
        let b = -10 + 3; # -> -7
        print(f"-10 + 3 = {b}");

        let c = 2 * 3; # -> 6
        print(f"2 * 3 = {c}");

        let d = 20 / 5; # -> 4
        print(f"20 / 5 = {d}");

        let e = 23 % 5; # -> 3
        print(f"23 % 5 = {e}");

        # Regular order of operations applies
        let f = 1 + 4 * 5; # -> 21
        print(f"1 + 4 * 5 = {f}");
    }

Note that due to Roto's static typing, we aren't allowed to do operations on
different integer types.

.. code-block:: roto

    fn main() {
        let a: u8 = 5;
        let b: u32 = 10;

        let c = a + b; # error!
    }

Floating point numbers can represent fractional numbers. There are two floating
point types: :roto:ref:`f32` and :roto:ref:`f64` which are 32- and 64-bit,
respectively. They support all the same operators as integers, except for ``%``.

To disambiguate floating point numbers from integers, you always have to add a
period to floating point literals.

.. code-block:: roto

    fn main() {
        # We can specify the type to get a specific integer type
        let a: f32 = 2.0 + 1.0; # -> 3.0
        print(f"2.0 + 1.0 = {a}");

        # We always have to write the period for floating point numbers
        let a: f32 = 2. + 1.; # -> 3.0
        print(f"2. + 1. = {a}");

        # If we don't specify the type we get f64
        let b = -12.3 + 4.5; # -> -7.8
        print(f"-10.0 + 3.0 = {b}");

        let c = 2.0 * 3.5; # -> 7.0
        print(f"2.0 * 3.0 = {c}");

        let d = 20.0 / 5.0; # -> 4.0
        print(f"20.0 / 5.0 = {d}");

        # Regular order of operations applies
        let f = 1.0 + 4.0 * 5.0; # -> 21.0
        print(f"1.0 + 4.0 * 5.0 = {f}");
    }
