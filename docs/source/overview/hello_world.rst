Hello, world!
=============

This is the all-important "Hello, world!" program in Roto:

.. code-block:: roto

  fn main() {
      print("Hello, world!");
  }

You can put this code in a file called ``hello_world.roto`` and then run it with
the following command:

.. code-block:: console

  $ roto run hello_world.roto

Which should show you the following output:

.. code-block::

  Hello, world!

There are a couple things we can learn from this program. First, a function is
defined with the ``fn`` keyword and we use curly braces to delimit the
function body. Second, functions (in this case ``print``) are called with
parentheses and string literals are enclosed in double quotes. Finally, a
statement in Roto ends with a semicolon.

Roto ignores whitespace as it parses the source code, so the indentation of the
second line is completely optional.

You can see Roto's type checker at work if you change the script to the following:

.. code-block:: roto

  fn main() {
      print(5);
  }

If you try to compile that script, Roto will show you the following error:

.. code-block::

  Error: Type error: mismatched types
     ╭─[ hello_world.roto:2:11 ]
     │
   2 │     print(5);
     │           ┬  
     │           ╰── expected `String`, found `{integer}`
  ───╯

This is a *compile-time* error, which is generated before the script is run.
This is part of the appeal of Roto: your scripts will be checked up front, so
they won't present problems later.

We can also use the compiler to only check our program without running it:

.. code-block:: console

  $ roto check hello_world.roto

Now you know how to run and check Roto scripts. You can look at the :doc:`../reference/language_reference`
to learn the rest of the language.
