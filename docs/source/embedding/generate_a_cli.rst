Generate a CLI
==============

.. note::
    This requires enabling the ``cli`` feature flag of the Roto crate.

The default Roto CLI introduced in :doc:`../overview/hello_world` is nice, but
does not know anything about custom types and functions added to the runtime.
To get a better CLI for your use case, you can generate a CLI from your runtime.

.. code-block:: rust

    let rt = Runtime::new();

    // add all your custom types, methods and constants here
    
    rt.cli();

The CLI helps with testing and type checking your scripts without running your
entire application. To start the CLI, just call the ``cli`` method on the 
``Runtime``. The commands of this CLI are identical to the ones described in
:doc:`../reference/command_line`.

