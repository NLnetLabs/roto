Setup
=====

This chapter will guide you through adding Roto scripting to your application.
We'll build a small application that loads a Roto script, compiles it and then
calls a function from it.

Requirements
------------

We're building a Rust application, so we need the Rust tooling, which is
available via `rustup <https://rustup.rs/>`_. That's all you need!


Initializing a project
----------------------

To get started run

.. code-block:: bash

    cargo new hello_roto

This will set up a new Rust project with the name `hello_roto`. You can `cd`
into the generated directory.


Adding the roto crate
---------------------

To use Roto in your Rust application, you need to depend on the ``roto`` crate.
This crate is available on `crates.io <https://crates.io>`_. So you can simply
add it to your ``Cargo.toml`` or add it using

.. code-block:: bash

    cargo add roto

Now you're all set up to get started!

Reading the docs
----------------

If you now run

.. code-block:: bash

    cargo doc --open

You will be able to see the documentation for the ``roto`` crate, which might
be helpful. The documentation is also available at
`docs.rs/roto <https://docs.rs/roto>`_.
