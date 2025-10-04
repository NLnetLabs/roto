Roto
====

.. note::

   Roto is under active development and the Roto syntax may change
   between 0.x versions.

   For more information
   on upcoming features and changes see the `issue tracker <https://github.com/
   NLnetLabs/roto/issues>`__.

Roto is an embedded scripting language for Rust applications that is fast, safe
and easy to use.

The language is primarily used by `Rotonda
<https://github.com/NLnetLabs/rotonda>`__, the composable, programmable routing
BGP engine. It is made to integrate especially well with Rotonda, so that
writing filters is as simple as possible. In addition, Roto can be easily
embedded into any Rust application for general purpose scripting.

Features
--------

 - Roto can be **embedded** into any Rust application. Rust types and functions
   can be registered for use in Roto.
 - Roto is **strongly and statically-typed**, ensuring that type errors are
   caught at compile-time. This does not mean that the user has to specify types
   everywhere, most types can be inferred by the Roto compiler. When the
   compiler detects a mistake in your script, it will emit a friendly message.
 - Scripts are **compiled** to machine code before they are executed. This means
   that they run quickly and introduce minimal latency into your system.
 - Roto scripts are **hot-reloadable**. The host application can recompile
   scripts at any time.

Limitations
-----------

These limitations are fundamental to the design of Roto. They stem from the
fact that Roto is a scripting language and that Rust's reflection system is
limited.

- All registered Rust types must implement ``Clone`` or ``Copy``. Rust types that
  don't implement these traits should be wrapped in an ``Rc`` or ``Arc``. The reason
  for this limitation is that Roto does not have references and freely clones
  values.
- It is not possible to register types that are not concrete. For example,
  ``Vec<u32>`` is possible, but ``Vec<T>`` is not. We plan to support registering
  generic via some form of type erasure.
- The parameter and return types of functions exported to the host application
  must have a ``'static`` lifetime.

Contributing
------------

If you have comments, proposed changes, or would like to contribute, please open
an issue in the `GitHub repository <https://github.com/NLnetLabs/roto>`__. In
particular, if you would like to use the crate but it is missing functionality
for your use case, we would love to hear from you!

License
-------

Roto is distributed under the terms of the BSD-3-clause license. See
`LICENSE <https://github.com/NLnetLabs/roto/blob/main/LICENSE>`__ for details.

   .. only:: html

      |mastodon|

      .. |mastodon| image:: https://img.shields.io/mastodon/follow/109262826617293067?domain=https%3A%2F%2Ffosstodon.org&style=social
         :alt: Mastodon
         :target: https://fosstodon.org/@nlnetlabs

.. toctree::
   :maxdepth: 2
   :hidden:
   :caption: Overview
   :name: toc-overview

   Introduction <self>
   overview/goals
   overview/installation
   overview/hello_world

.. toctree::
   :maxdepth: 2
   :hidden:
   :caption: Embedding Roto
   :name: toc-embedding

   embedding/setup
   embedding/call_a_function
   embedding/expand_the_runtime
   embedding/generate_a_cli

.. toctree::
   :maxdepth: 2
   :hidden:
   :caption: Reference
   :name: toc-reference

   reference/syntax_overview
   reference/language_reference
   reference/std/index
   reference/rust_interoperability
   reference/command_line
   API documentation (docs.rs) <https://docs.rs/roto>
