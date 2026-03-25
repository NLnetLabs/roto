Control Flow
============

After introducing the basic types and operations, it's time we delve into more
complex programs with control flow.

If-else
-------

An ``if-else`` expression allows you to branch based on a boolean value. In
contrast with some other languages, Roto does not require parentheses around
the condition, but it does require curly braces for the body.  You can use the
``if-else`` expression as a statement. The ``else`` part is then optional.

.. code-block:: roto

    fn main() {
        let x = 100;
        if x % 2 == 0 {
            print("x is even");
        } else {
            print("x is odd");
        }
    }

.. testoutput::

    x is even

An ``if-else`` expression also evaluates to a value itself. That value is determined
by the last value in each of the arms, as long as there is no semicolon after them.

.. code-block:: roto

    fn main() {
        let x = 100;
        let sign = if x > 0 {
            print("x is positive");
            1
        } else {
            print("x is negative");
            -1
        };
        print(f"{sign}");
    }

.. testoutput::

    x is positive
    1

While loops
-----------

.. code-block:: roto

    # Euclidean algorithm for greatest common divisor
    fn main() {
        let a_initial = 125;
        let b_initial = 50;

        let a = a_initial;
        let b = b_initial;
        while b != 0 {
            let t = b;
            b = a % b;
            a = t;
        }

        print(f"gcd({a_initial}, {b_initial}) = {a}")
    }

.. testoutput::

    gcd(125, 50) = 25

For loops
---------

TODO

