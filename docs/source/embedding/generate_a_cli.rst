Generate a CLI
==============

Roto can generate a CLI for any runtime. To do this, you need to enable the
``cli`` feature flag on the crate.

The CLI helps with testing and type checking your scripts without running your
entire application. To start the CLI, just call the ``cli`` method on the 
``Runtime``. The commands of this CLI are identical to the ones described in
:doc:`../reference/command_line`.

.. code-block:: rust

    let runtime = Runtime::new();
    runtime.cli();
