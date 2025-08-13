Goals
=====

By now, you might like the idea of Roto, but you're not sure whether it is the
right fit for your project. To help you figure that, we explain some of the
design goals and their consequences below.

Easy to learn & use
-------------------

Experienced programmers should be able to pick up easily. Since it is a new
language, we are careful to not make it too surprising for most users. We try
to use familiar symbols and keywords for common operations that most users will
recognize. We want the compiler to emit clear error messages that help users
diagnose their problems. We do not want to add concepts that many programmers
might not be familiar with, especially when coming from scripting languages. An
example of this is that Roto is garbage collected and no ownership concepts
like Rust.

Designed to be integrated
-------------------------

Any Rust application should be able to add support for Roto scripts with
minimal hassle. Roto can be added to a project simply by adding the crate as
a dependency to a project. We do not want to require the presence of any other
libraries or executables. We want it to be easy to expose Rust types and
functions to Roto scripts. Roto doesn’t produce its own binaries, but is always
embedded into some host application. Since the host application decides what to
expose to the scripts, we do not have an extensive standard library.

Reliable
--------

We want you to be able to rely on a Roto script once it is compiled. It should
be possible to use Roto in critical software that needs to be run reliably. To
achieve this, Roto is statically typed and memory-safe. We also do not want it
to crash the process.

Performant
----------

We want Roto to be faster than most other embedded scripting languages.
However, we do not need to compete with other statically typed and compiled
languages. Roto will be most suited for applications that compile a script once
and then run that many times.
