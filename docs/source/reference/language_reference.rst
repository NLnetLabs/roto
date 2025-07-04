Language Reference
==================

This section describes the basic syntax of Roto scripts. This is written in
a reference-style. It is mostly meant as a cheatsheet, not as an introduction to
the language.

Comments
--------

Comments in Roto start with a ``#`` and continue until the end of a line. They can
be inserted anywhere in the script and are ignored.

.. code-block:: roto

    # this is a comment

Block comments are not supported. To create a multiline comment, prefix every
line with a ``#``.

.. code-block:: roto

    # one comment line
    # another comment line

Literals
--------

Roto supports literals for primitive types:

- ``0``, ``1``, ``34``, ``-10``, ``0xFF`` etc. for integers
- ``true`` and ``false`` for booleans
- ``0.0.0.0``, ``2345:0425:2CA1:0000:0000:0567:5673:23b5``, ``0::``, etc.
  for IP addresses
- ``0.0.0.0/10`` for prefixes
- ``AS1234`` for AS numbers
- ``"Hello"`` for strings

Primitive types
---------------

There are several types at Roto's core, which can be expressed as literals.

- :roto:ref:`bool`: booleans
- :roto:ref:`u8`, :roto:ref:`u16`, :roto:ref:`u32`, :roto:ref:`u64`: unsigned integers of 8, 16, 32 and 64 bits, respectively
- :roto:ref:`i8`, :roto:ref:`i16`, :roto:ref:`i32`, :roto:ref:`i64`: signed integers of 8, 16, 32 and 64 bits, respectively
- :roto:ref:`IpAddr`: IP address
- :roto:ref:`Prefix`: prefixes
- :roto:ref:`Asn`: AS number
- :roto:ref:`String`: Strings

There are many more types available that have more to do with BGP. These are
described elsewhere. Note that Roto is case sensitive; writing the ``Asn`` type as
``ASN`` or ``asn`` won't work.

Integers
--------

As the previous section indicates, there are several types for integers in Roto.
This might be familiar to users of languages such as C and Rust, but not for
users of Python and similar languages which only have one integer type.

Roto is a compiled language and as such needs to know how many bytes to use for
a given integer. Hence, the number of bits are included in the type. The prefix
``u`` is used for unsigned (i.e. non-negative) numbers and ``i`` for signed integers.

Below is a table of all available integer types.

+-----------------+------+--------+----------------------------+----------------------------+
| Type            | Bits | Signed |                        Min |                        Max |
+=================+======+========+============================+============================+
| :roto:ref:`u8`  |    8 |     No |                          0 |                         255|
+-----------------+------+--------+----------------------------+----------------------------+
| :roto:ref:`u16` |   16 |     No |                          0 |                     65,535 |
+-----------------+------+--------+----------------------------+----------------------------+
| :roto:ref:`u32` |   32 |     No |                          0 |              4,294,967,295 |
+-----------------+------+--------+----------------------------+----------------------------+
| :roto:ref:`u64` |   64 |     No |                          0 | 18,446,744,073,709,551,615 |
+-----------------+------+--------+----------------------------+----------------------------+
| :roto:ref:`i8`  |    8 |    Yes |                       -128 |                         127|
+-----------------+------+--------+----------------------------+----------------------------+
| :roto:ref:`i16` |   16 |    Yes |                     -32768 |                     65,535 |
+-----------------+------+--------+----------------------------+----------------------------+
| :roto:ref:`i32` |   32 |    Yes |                -2147483648 |              4,294,967,295 |
+-----------------+------+--------+----------------------------+----------------------------+
| :roto:ref:`i64` |   64 |    Yes | -9,223,372,036,854,775,808 |  9,223,372,036,854,775,807 |
+-----------------+------+--------+----------------------------+----------------------------+

Arithmetic operators
--------------------

There are mathematical operators for common operations:

+-------+----------------+
| ``+`` | addition       |
+-------+----------------+
| ``-`` | subtraction    |
+-------+----------------+
| ``*`` | multiplication |
+-------+----------------+
| ``/`` | division       |
+-------+----------------+

These operators follow the conventional PEMDAS rule for precedence. The order is

- Parentheses
- Multiplication and division
- Addition and subtraction

Parentheses can always be used to force a certain order of operations. For
example, this expression:

.. code-block:: roto

    1 + 2 * 3    # evaluates to 7

is interpreted as

.. code-block:: roto

    1 + (2 * 3)  # evaluates to 7

and not as

.. code-block:: roto

    (1 + 2) * 3  # evaluates to 9

Comparison operators
--------------------

In addition to arithmetic operators, there are operators to compare values.
Comparison operators have a lower precedence than arithmetic operators. The
script won't compile if the operands have different types.

+--------+-----------------------+
| ``==`` | Equals                |
+--------+-----------------------+
| ``!=`` | Does not equal        |
+--------+-----------------------+
| ``>``  | Greater than          |
+--------+-----------------------+
| ``>=`` | Greater than or equal |
+--------+-----------------------+
| ``<``  | Less than             |
+--------+-----------------------+
| ``<=`` | Less than or equal    |
+--------+-----------------------+

Examples:

.. code-block:: roto

    5 > 10      # evaluates to false
    10 > 5      # evaluates to true
    5 == 5      # evaluates to true
    5 == true   # compile error!
    1 < x < 10  # compile error!

Logical operators
-----------------

Operators to combine boolean values are called logical operators. They have a
lower precedence than comparison operators. These are the logical operators in
Roto:

+---------+-------------+
| ``&&``  | Logical and |
+---------+-------------+
| ``||``  | Logical or  |
+---------+-------------+
| ``not`` | Negation    |
+---------+-------------+

Now that we have all the rules for precendence, here is an example using all types of
operators (arithmetic, comparison and logical):

.. code-block:: roto

    1 + x * 3 == 5 && y < 10

This is equivalent to:

.. code-block:: roto

    ((1 + (x * 3)) == 5) && (y < 10)

Strings
-------

Strings are enclosed in double quotes like so:

.. code-block:: roto

    "This is a string!"

Strings can be concatenated with ``+``:

.. code-block:: roto

    "race" + "car" # yields the string "racecar"

It also has some methods such as :roto:ref:`String.contains` that can be very
useful. See the documentation for the :roto:ref:`String` type for more
information.

If-else
-------

To conditionally execute some code, use an ``if`` block. The braces in the
example below are required. The condition does not require parentheses. The
condition must evaluate to a boolean.

.. code-block:: roto

    if x > 0 {
        # if the condition is true
    }

An ``else``-clause can optionally follow the ``if``-block. The ``if``-``else``
construct is an expression and therefore evaluates to a value.

.. code-block:: roto

    if x > 0 {
        # if the condition is true
    } else {
        # if the condition is false
    }

Functions
---------

Functions can be defined with the ``fn`` keyword, followed by the name
and parameters of the function. It is required to specify the types of the
parameters. The return type is specified with ``->``. A function without a
return type does not return anything.

.. code-block:: roto

    fn add_one(x: u64) -> u64 {
        x + 1
    }

This function can then be called like so:

.. code-block:: roto

    add_one(10)

A function can contain multiple expressions. The last expression is returned if
it is not terminated by a ``;``. The return can also be made explicit with the
``return`` keyword. This function is equivalent to the previous example. 

.. code-block:: roto

    fn add_one(x: u64) -> u64 {
        return x + 1;
    }

The following function uses multiple statements to return ``0`` if the input is ``0``
and subtract ``1`` otherwise.

.. code-block:: roto

    fn subtract_one(x: u64) -> u64 {
        if x == 0 {
            return 0;
        }
        x - 1
    }

This function does not return anything:

.. code-block:: roto

    fn returns_nothing(x: u64) {
        x + 1;
    }

The ``return`` keyword can still be used in functions that don't return a value to
exit the function early.

When a function becomes more complex, intermediate results can be stored in local
variables with ``let``.

.. code-block:: roto

    fn greater_than_square(x: i32, y: i32) {
        let y_squared = y * y;
        x > y_squared
    }

Any local variable can be overwritten with an assignment, which is expressed as ``=``
without `let`:

.. code-block:: roto

    let x = 0;
    x = x + 1;

Filter-map
----------

A ``filtermap`` is a function that filters and transforms some incoming value.

Filter-maps resemble functions but they don't ``return``. Instead they
either ``accept`` or ``reject``, which determines what happens to the value.
Generally, as accepted value is stored or fed to some other component and a
reject value is dropped.

.. code-block:: roto

    filtermap reject_zeros(input: IpAddr) {
        if input == 0.0.0.0 {
            reject
        } else {
            accept
        }
    }

This describes a filter which takes in an IP address and accepts it if it is not
equal to ``0.0.0.0``.

Like with functions, intermediate results can be stored in variables with let
bindings.

.. code-block:: roto

    filtermap reject_zeros(input: IpAddr) {
        let zeros = 0.0.0.0;
        if input == zeros {
            reject
        } else {
            accept
        }
    }

A ``filtermap`` can also ``accept`` or ``reject`` with a value.

Anonymous records
-----------------

Multiple values can be grouped into records. A record is constructed with `{}`
and contains key-value pairs.

.. code-block:: roto

    { foo: 5, bar: 10 }

These records are statically typed, which means that records with different
field names or different field types are separate types. For example, this is
a type checking error:

.. code-block:: roto

    if x {
        { foo: 5, bar: 10 }
    } else {
        { foo: 5 }  # error!
    }

Note that this makes records significantly different from dictionaries in Python
and objects in JavaScript, which resemble hash-maps and are far more dynamic.

Fields of records can be accessed with the `.` operator.

.. code-block:: roto

    filtermap example_filter_map() {
        let x = { foo: 5 };
        accept x.foo
    }

Fields can also be updated with an assignment.

.. code-block:: roto

    let x = { foo: 5 };
    x.foo = 6;

Named records
-------------

Named records provide a more principled approach to grouping values which will
yield more readable type checking errors.

.. code-block:: roto

    type SomeRecord {
        foo: i32,
        bar: bool,
    }

    # ...

    x = SomeRecord { foo: 3, bar: false }

Roto checks that all declared values are provided and are of the same type.

There is an automatic coercion from anonymous records to named records:

.. code-block:: roto

    fn foo(int: i32) -> SomeRecord {
        { foo: int, bar: false }  # implicitly coerced to SomeRecord
    }

Modules
-------

A Roto script can be split over multiple files. To do this, we have to create
a folder with the name of the script and create a Roto file directly in it
called ``pkg.roto``. This file is the root of our script. The contents of
``pkg.roto`` will form the ``pkg`` module. No other files in the directory 
can be called ``pkg.roto``.

Files adjacent to ``pkg.roto`` are submodules of ``pkg``. For example, a file
called ``foo.roto`` will define the module ``pkg.foo``.

A directory next to ``pkg.roto`` will also be a submodule if it contains a file
called ``lib.roto``. A file ``foo/lib.roto`` is therefore equivalent to ``foo.roto``
and defines the module called ``pkg.foo``. We can do this recursively, so we can
define the module ``pkg.foo.bar`` with either a file called ``foo/bar.roto`` or
``foo/bar/lib.roto`` and so forth.

The files ``foo.roto`` and ``foo/lib.roto`` cannot both exist and only
``foo/lib.roto`` can have submodules.

An item such as a function, filtermap or type can be used from other modules in
a couple of ways. To access them, we must know the path, which is the
period-separated list of identifiers to follow to get to the item, starting with
the module name and ending with the name of the item.

For the following examples, we will work with the following files:

.. code-block::

    pkg.roto
    foo.roto
    bar/lib.roto
    bar/baz.roto

These define the following modules:

.. code-block::

    pkg
    pkg.foo
    pkg.bar
    pkg.bar.baz

Now assume that ``foo.roto`` contains a function called ``square``, this
function can be referenced in any of the other files with the absolute path
``pkg.foo.square``. For example:

.. code-block:: roto

    fn add_and_square(x: i32, y: i32) -> i32 {
        pkg.foo.square(x + y)
    }

We can also use the relative path, which is different for each file. We can use
the ``super`` keyword in a path to reference the parent module of the current
module. Multiple ``super`` keywords can appear at the start of a path.

.. code-block:: roto

    # in pkg.roto   
    foo.square

    # in foo.roto
    square

    # in bar.roto
    super.foo.square

    # in bar/baz.roto
    super.super.foo.square

There are 3 special identifiers that can only be used at the start of a path
and automatically make the path an absolute path:

- ``pkg`` for the current package 
- ``std`` for the Roto standard library
- ``dep`` for dependencies (not implemented yet, but the identifier is reserved)

Imports
-------

Of course, writing out the full path to anything you want to use can become
quite tedious. We can import items from other modules into the current module
with the ``import`` keyword. The ``import`` keyword is followed by a path. The
item the path references will be available by name in the current scope.

.. code-block:: roto
    
    import foo.square;

    fn fourth_power(x: i32) -> i32 {
        square(square(x))
    }

We can also import entire modules. Imported modules are not available in other
modules.

An ``import`` does not need to be at the top-level, they can be in any scope.
We can rewrite the previous example as follows.

.. code-block:: roto
 
    fn fourth_power(x: i32) -> i32 {
        import foo.square;
        square(square(x))
    }

Now the name `square` can only be used within the `fourth_power` function and
not in any other functions we define. But we can define even more granular
imports such as in the following example, where we use a function ``foo`` from
either module ``A`` or ``B``, depending on a boolean flag.

.. code-block:: roto

    fn use_foo(x: i32, choice: bool) -> i32 {
        if choice {
            import A.foo;
            foo(x)
        } else {
            import B.foo;
            foo(x)
        }
    }

Next steps
----------

You can learn more about Roto by looking at the documentation for the
:doc:`std`.
