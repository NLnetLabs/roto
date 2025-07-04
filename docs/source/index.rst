Roto |version|
=================

.. note::

   ``Roto`` is under active development and the ``Roto`` syntax may change
   between 0.x versions.

   For more information
   on upcoming features and changes see the `ROADMAP <https://github.com/
   NLnetLabs/rotonda/blob/main/ROADMAP.md>`__.

Roto is an embedded scripting language that is fast, safe and easy to use.

Primarily used by Rotonda, the composable, programmable routing BGP engine. It
is made to integrate especially well with Rotonda, so that writing filters is as
simple as possible. In addition, Roto can be easily embedded into any Rust
application for general purpose scripting.

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

Contributing
------------

If you have comments, proposed changes, or would like to contribute, please open
an issue in the GitHub repository. In particular, if you would like to use the
crate but it is missing functionality for your use case, we would love to hear
from you!

License
-------

The roto crate is distributed under the terms of the BSD-3-clause license. See
LICENSE for details.

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

   reference/language_reference
   reference/std
   reference/rust_interoperability
   reference/command_line
   API documentation (docs.rs) <https://docs.rs/roto>
