Language Reference
==================

This section describes the basic syntax of Roto scripts. This is written in
a reference-style. It is mostly meant as a cheatsheet, not as an introduction to
the language.

.. _lang_comments:

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
- ``0.0``, ``10.``, ``10e5``, ``5E-5`` etc. for floating point numbers
- ``true`` and ``false`` for booleans
- ``0.0.0.0``, ``2001:DB8:2CA1:0000:0000:0567:5673:23b5``, ``::``, etc.
  for IP addresses
- ``0.0.0.0/10`` for prefixes
- ``AS1234`` for AS numbers
- ``"Hello"`` for strings

.. note::
    Floating point literals need either a ``.``, ``e`` or ``E`` to distinguish
    them from integer literals.

Identifiers
-----------

Identifiers in Roto can consist of any character from Unicode's ``XID_Start``
character set or an ``_`` followed by characters from ``XID_Continue``. In
practice, this means that identifiers can use ASCII, numbers, diacritics and
other alphabets, with the restriction that they cannot *start* with a number.
Keywords are also not valid identifiers.

Identifiers are case-sensitive, so ``foo`` is not considered to be equal to
``Foo`` or ``FOO``. Every character in the identifier is significant.

Here are some examples of valid identifiers:

- ``foo``
- ``_bar``
- ``foo_bar``
- ``foo1234``
- ``Straße``
- ``Москва``
- ``東京``

The following strings are **not** valid identifiers:

- ``12foo`` (cannot start with a number)
- ``foo.bar`` (cannot contain ``.``)
- ``filter`` (cannot be a keyword)

Additionally, we have the following conventions:

- The names of local variables, modules, functions and function arguments
  should use ``snake_case``, i.e. should be all lowercase with words separated by ``_``.
- The names of types and variant constructors should use ``PascalCase``, i.e. should
  have each word capitalized. The exceptions are the primitive boolean, integer
  and floating point types.
- The names of constants should use ``SCREAMING_SNAKE_CASE``, i.e. should be all uppercase
  with words separated by ``_``.
- A leading underscore can be used to signal that a value is unused.

Primitive types
---------------

There are several types at Roto's core, which can be expressed as literals.

- :roto:ref:`bool`: boolean
- :roto:ref:`u8`, :roto:ref:`u16`, :roto:ref:`u32`, :roto:ref:`u64`: unsigned integer of 8, 16, 32 and 64 bits, respectively
- :roto:ref:`i8`, :roto:ref:`i16`, :roto:ref:`i32`, :roto:ref:`i64`: signed integer of 8, 16, 32 and 64 bits, respectively
- :roto:ref:`f32`, :roto:ref:`f64`: floating point numbers of 32 and 64 bits, respectively
- :roto:ref:`String`: string
- :roto:ref:`IpAddr`: IP address
- :roto:ref:`Prefix`: IP prefix
- :roto:ref:`Asn`: AS number

There are many more types available that have more to do with BGP. These are
described elsewhere. Note that Roto is case-sensitive; writing the ``String`` type as
``STRING`` or ``string`` won't work.

.. _lang_unit:

Unit type
---------

The unit type is a special type written as ``()`` with only one value: ``()``.
It is the type of expressions that do not have meaningful values to evaluate to.
For functions, returning ``()`` is equivalent to returning nothing.

.. _lang_never:

Never type
----------

The never type ``!`` is an *uninhabited* type, meaning that it cannot be
constructed. It appears in code paths that are unreachable. For example, it
is the type of a ``return`` expression. It can be unified with any other type.

.. _lang_booleans:

Booleans
--------

The boolean type in Roto is called :roto:ref:`bool` and it has two possible
values: `true` and `false`. Booleans can be manipulated via several operators
such as `&&` (logical and), `||` (locical or) and `not` (logical negation).

.. _lang_integers:

Integers
--------

There are several types for integers in Roto.
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

.. _lang_floats:

Floating point numbers
----------------------

There are two floating point types: ``f32`` and ``f64``, of 32 and 64 bits
length, respectively.

+-----------------+------+
| Type            | Bits |
+=================+======+
| :roto:ref:`f32` |   32 |
+-----------------+------+
| :roto:ref:`f64` |   64 |
+-----------------+------+

.. _lang_arithmetic:

Arithmetic operators
--------------------

The unary ``-`` operator will negate a number. It requires that its operand is
a signed integer or a floating point number (i.e. not an unsigned integer).

There are binary operators for common arithmetic operations, which are
implemented for all numeric types (integers and floating point numbers):

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

+---------+--------------------------------+
| ``&&``  | Logical and (short-circuiting) |
+---------+--------------------------------+
| ``||``  | Logical or (short-circuiting)  |
+---------+--------------------------------+
| ``not`` | Negation                       |
+---------+--------------------------------+

Now that we have all the rules for precedence, here is an example using all types of
operators (arithmetic, comparison and logical):

.. code-block:: roto

    1 + x * 3 == 5 && y < 10

This is equivalent to:

.. code-block:: roto

    ((1 + (x * 3)) == 5) && (y < 10)

The ``&&`` and ``||`` are short-circuiting, meaning that if the left-hand operand
of ``&&`` evaluates to ``false`` or the left-hand operand of ``||`` evaluates to
``true``, the right hand side won't be evaluated.

.. _lang_strings:

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

Escape sequences
----------------

Strings can contain the following escape sequences:

+-----------------+--------------------------+-----------------+
| Escape sequence |      Escaped value       |   Common name   |
+=================+==========================+=================+
| ``\0``          | U+0000 (NUL)             | Nul             |
+-----------------+--------------------------+-----------------+
| ``\t``          | U+0009 (HT)              | Tab             |
+-----------------+--------------------------+-----------------+
| ``\n``          | U+000A (LF)              | Newline         |
+-----------------+--------------------------+-----------------+
| ``\r``          | U+000D (CR)              | Carriage return |
+-----------------+--------------------------+-----------------+
| ``\"``          | U+0022 (QUOTATION MARK)  | Double quote    |
+-----------------+--------------------------+-----------------+
| ``\'``          | U+0027 (APOSTROPHE)      | Single quote    |
+-----------------+--------------------------+-----------------+
| ``\\``          | U+005C (REVERSE SOLIDUS) | Backslash       |
+-----------------+--------------------------+-----------------+

In addition, any unicode character can be represented by its scalar value. This
can be done with `\x` followed by 2 hexadecimal digits or with `\u{...}` where
the `...` is a hexadecimal number.

Finally, Roto will ignore any whitespace after a ``\`` followed by a newline.

.. _lang_string_formatting:

String formatting
-----------------

Roto supports a Python-like syntax for string formatting. Any string literal
prefixed with `f` will become a format string (or "f-string"), that interpolates
the expressions between ``{`` and ``}``. The f-string will insert a call to the
``to_string`` method for displaying the value. Therefore, any type with a
``to_string`` method can be put in an f-string, including registered types.

.. code-block:: roto

    let x = 10;
    print(f"x is {x}"); # will print "x is 10"

Arbitrary expressions are allowed to appear in format strings, including other
strings and format strings.

.. code-block:: roto

    let x = 10;
    print(f"Twice x is {2 * x}");

    print(f"x is {if x > 100 {
        "big"
    } else {
        "small"
    }}");

The ``{`` and ``}`` characters need to be escaped to be used in an f-string by
duplicating them: ``{{``, ``}}``.

.. code-block:: roto

    print(f"x is {{ x }}")
    # will print the string "x is { x }"

.. _lang_locals:

Local variables
---------------

Local variables are declared with a ``let`` statement.

.. code-block:: roto

    fn greater_than_square(x: i32, y: i32) {
        let y_squared = y * y;
        x > y_squared
    }

Any local variable can be overwritten with an assignment, which is expressed as ``=``
without ``let``:

.. code-block:: roto

    let x = 0;
    x = x + 1;

A let-binding can take an optional type annotation for readability or to help
with type inference. In the example below, ``0`` has an unknown type as it can
be any integer type, so the type annotation forces it to be ``u32``.

.. code-block:: roto

    let x: u32 = 0;

Local variables are dropped (i.e., deleted) at the end of the scope where they
are declared. A new scope is created with ``{}``, including when that is part of
the syntax. For example, the body of an ``if`` expression creates a new scope.

.. code-block:: roto

    let x = true;
    if x {
        let y = false;
        print(f"{y}"); # ok!
        # y is implicitly dropped here
    }
    print(f"{x}"); # ok!
    print(f"{y}"); # this is not possible: y has been dropped!

.. _lang_if_else:

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

The if-else is an expression, not a statement, which means that it evaluates to
a value. This means that it can be used as a replacement for a ternary
operator.

.. code-block:: roto

    let x = if y { 1 } else { 0 };

If-else expressions can be chained without additional braces.

.. code-block:: roto

    if x > 0 {
        print("x is positive!");
    } else if x < 0 {
        print("x is negative!");
    } else {
        print("x is zero!");
    }

.. _lang_match:

Match
-----

Pattern matching in Roto is supported via ``match`` expressions. These take a
value and a set of patterns to check against, with an expression associated with
each of the patterns.

The pattern is separated from the associated expression with ``->``. The arms
should be separated with commas, unless the expression is a block, i.e., when
it is wrapped in ``{}``.

The current implementation of this feature is very limited: you
can only match against ``variant`` types and only match against the
constructor, not against the contents of the constructor.  See `issue 124
<https://github.com/NLnetLabs/roto/issues/124>`_ for the status on these
limitations.

.. code-block:: roto

    let x = Some(10);
    match x {
        None -> print("x is None"),
        Some(i) -> {
            print("x is Some");
            print(f"x is {i}");
        }
    }

.. note::
    If you are used to Rust, be aware that Roto uses ``->`` instead of ``=>`` to
    separate the pattern from the expression.

    Another difference to be aware of is that Roto currently doesn't use
    the full path to the ``variant`` constructor, but only the name. So
    ``Option.None`` is not allowed as a pattern, but ``None`` is. This will
    probably change once ``match`` expressions become more general.


.. _lang_while:

While loops
-----------

A while loop takes a condition and a block. It will keep executing the block
until the condition evaluates to ``false``.

.. code-block:: roto

    let i = 0;
    while i < 10 {
        i = i + 1;
    }

A while loop is an expression of the type ``()``. Like with ``if``, ``while``
does not require parentheses around the condition.

.. _lang_functions:

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


.. _lang_filtermap:

Filtermap
---------

A ``filtermap`` is a function that filters and transforms some incoming value.

Filter-maps resemble functions but they don't ``return``. Instead they
either ``accept`` or ``reject``, which determines what happens to the value.
Generally, an accepted value is stored or fed to some other component and a
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

.. code-block:: roto

    filtermap small_enough(x: i32) {
        if x < 10 {
            accept x
        } else {
            reject "value was too big!"
        }
    }

This ``filtermap`` is identical to the following function:

.. code-block:: roto

    fn small_enough(x: i32) -> Verdict[i32, String] {
        if x < 10 {
            return Verdict.Accept(x)
        } else {
            return Verdict.Reject("value was too big!")
        }
    }

On the Rust side, a filtermap is a function that returns a ``Verdict<A, R>``.
The type parameters of a ``Verdict`` specify the types of the values given in
the ``accept`` and ``reject`` cases, respectively.

.. _lang_anonymous_records:

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

.. _lang_named_records:

Named records
-------------

Named records provide a more principled approach to grouping values which will
yield more readable type checking errors.

.. code-block:: roto

    record SomeRecord {
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

.. _lang_variant_type:

Variant types
-------------

Another kind of custom type in Roto are ``variant`` types. These types have a
set of constructors and values of these types are always constructed using one
of these. Each of the constructors can take arguments. To inspect ``variant``
types, we can ``match`` on them.

.. code-block:: roto

    # A `Number` variant type that has the constructors `Int`, `Float` and `Nan`.
    variant Number {
        Int(i32),
        Float(f32),
        Nan,
    }

    fn use_number() {
        let x = Number.Int(10);
        let y = Number.Float(21.5),
        let z = Number.Nan;

        match x {
            Int(i) -> print(f"int: {i}"),
            Float(f) -> print(f"float: {f}"),
            Nan -> print(f"nan!"),
        }
    }

Variant types can be generic over other types by taking type parameters.

.. code-block:: roto

    variant Either[L, R] {
        Left(L),
        Right(R),
    }

    fn to_string(x: Either[i32, String]) -> String {
        match x {
            Left(i) -> f"{i}",
            Right(s) -> s,
        }
    }

.. note::
    If you're familiar with Rust, ``variant`` types are just ``enum`` with another name.
    Or, for the functional programmers, ``variant`` types are algebraic data types.

.. note::
    Variant types are also used to model optional values. See :ref:`lang_optionals`.

.. _lang_lists:

Lists
-----

A list is a growable array of items. In Roto, all elements of a list must be of the
same type. Therefore, the type of a list has a type parameter: ``List[T]``. You can
create lists with ``[`` and ``]`` with expressions separated by commas.

.. code-block:: roto

  let my_list: List[i32] = [1, 2, 3];
  let first: i32? = my_list.get(0);
  match first {
      Some(first) -> print(f"First element was: {first}"),
      None -> print("No elements!"),
  }

The ``+`` operator can be used to concatenate two lists.

.. code-block:: roto

  let a = ["one", "two", "three"];
  let b = ["four", "five", "six"];
  let concatenated = a + b;

See :roto:ref:`List[T]` for all methods available on lists. Lists are passed by reference,
meaning that if we assign a list ``a`` to a variable ``b`` and we modify ``a`` then ``b``
will see the same modifications.

Modules
-------

A Roto script can be split over multiple files. To do this, we have to create
a folder with the name of the script and create a Roto file directly in it
called ``pkg.roto``. This file is the root of our script. The contents of
``pkg.roto`` will form the ``pkg`` module. No other files in the directory
can be called ``pkg.roto``.

Files adjacent to ``pkg.roto`` are sub-modules of ``pkg``. For example, a file
called ``foo.roto`` will define the module ``pkg.foo``.

A directory next to ``pkg.roto`` will also be a sub-module if it contains a file
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

.. _lang_imports:

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

.. _lang_optionals:

Optional values
---------------

Roto does not feature a value like ``None``, ``null`` or ``nil``. Instead, it
has optional values. The type of an optional value is written ``T?``, which is
shorthand for ``Option[T]``. For example, an optional ``u32`` is ``u32?``, or
equivalently, ``Option[u32]``.

The ``Option`` type is a ``variant`` type with 2 constructors: ``None`` and
``Some``. A value of ``T?`` is constructed with either ``Option.None`` or
``Option.Some(t)`` where ``t`` is a value of type ``T``.

Like any variant type it is possible to match on a value of type ``T?``

.. code-block:: roto

    match x {
        Some(x) -> x,
        None -> 0,
    }

In addition, there is a ``?`` operator, which will evaluate to the value of
``Some`` or return ``Option.None``. That is, if ``x`` is of type ``T?``, then
``x?`` is equivalent to the following match expression:

.. code-block:: roto

    match x {
        Some(x) -> x,
        None -> return Option.None,
    }

Tests
-----

Tests are written with the ``test`` keyword followed by an identifier and a block.
The name must be unique. Inside the block you can write any Roto code that you
want but it must return a ``Verdict[(), ()]``. If the test returns with
``accept``, the test passes, it it returns with ``reject``, the test fails.

Tests look a lot like functions with a few crucial differences: they cannot be
called directly and they do not have any arguments. Instead, Roto's test runner
finds the tests and runs them.

.. code-block:: roto

    fn add(x: u32, y: u32) -> u32 {
        x + y
    }

    test test_add_function {
        if add(10, 20) == 30 {
            accept
        } else {
            reject
        }
    }

If you're using Roto as a binary or with the `generated CLI <generate_cli>`__,
you can run the tests with the ``test`` subcommand:

.. code-block:: console

    $ roto test

Next steps
----------

You can learn more about Roto by looking at the documentation for the
:doc:`std/index`.
