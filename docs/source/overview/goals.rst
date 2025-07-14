Goals
=====

There are several pillars that the design of based on:

1. Scripts should be memory-safe, but users should not have to worry about
   ownership and borrowing.
2. Syntax should be easy to pick up. The "weirdness budget" is low.
3. We target runtime performance faster than most scripting languages
   (e.g. lua), but don't have to compete with Rust and C in terms of
   performance.
4. Roto is designed as a general purpose language, with some extra features for
   Rotonda.
5. Roto should be able to compile scripts quickly.
6. Roto should be easy to package as a Rust crate.
7. Roto should give friendly and helpful error messages.

Roto is an embedded scripting language. This means that it doesn't produce its
own binaries, but is always embedded into some host application.

In practice, these pillars lead to the following decisions:

- We use cranelift as a backend, instead of LLVM. (3, 5 & 6)
- We use an imperative C-style syntax. (2 & 4)
- We pass variables by value and do not allow pointers. (1)

For the design, we draw inspiration mostly from Rust, Python & Gleam.
