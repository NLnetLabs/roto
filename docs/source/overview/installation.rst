Installation
============

Roto is meant to be included into a host application as a crate, to learn how
to do that, see :doc:`../embedding/setup`. In that case, you don't need a separate
compiler to use Roto. However, there is also a binary of the Roto
compiler that you can use to play around with the language.

Installing the compiler
-----------------------

The standalone compiler can be installed using cargo:

.. code-block:: console

  cargo install --locked roto

Editor support
--------------

Editors with tree-sitter support
""""""""""""""""""""""""""""""""

Here is a list of known third party extensions for adding Roto highlighting to other editors:

- `Zed <https://zed.dev/extensions/roto>`__

A tree-sitter grammar for Roto is available in the `tree-sitter-roto repository
<https://github.com/NLnetLabs/tree-sitter-roto>`__. If your editor supports tree-sitter
you can install it from that repository.

Here is the relevant documentation on adding a new language for a few common editors:

- `Helix <https://docs.helix-editor.com/master/languages.html#tree-sitter-grammar-configuration>`__
- `Neovim <https://neovim.io/doc/user/treesitter.html#_parser-files>`__
- `Zed <https://zed.dev/docs/extensions/languages#grammar>`__

.. note::

  We plan on implementing an LSP for Roto in the future. We will unfortunately
  not be able to maintain plugins for editors that do not support tree-sitter
  and LSP.

Sublime Text
""""""""""""

For Roto syntax highlighting we provide the syntax file `roto.sublime-syntax <https://github.com/NLnetLabs/roto/blob/main/tooling/sublime/roto.sublime-syntax>`__. Grab a copy of it and place it inside your users Sublime Text packages configuration folder:

.. code-block:: console

  curl https://raw.githubusercontent.com/NLnetLabs/roto/refs/heads/main/tooling/sublime/roto.sublime-syntax > ~/.config/sublime-text/Packages/User/roto.sublime-syntax

Roto should now be available for highlighting. For further information take a look at the `Sublime Text documentation <https://www.sublimetext.com/docs/packages.html>`__.
