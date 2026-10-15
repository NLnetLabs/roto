# Setup

This chapter will guide you through adding Roto script execution to your
application. We'll build a small application that loads a Roto script, compiles
it and then calls a function from it.

## Requirements

We're building a Rust application, so we need the Rust tooling, which is
available via [rustup](https://rustup.rs/). That's all you need!


## Initializing a Project

To get started run

```console
$ cargo new hello_roto
```

This will set up a new Rust project with the name `hello_roto`. You can `cd`
into the generated directory.


## Adding the Roto Crate

To use Roto in your Rust application, you need to depend on the `roto` crate.
This crate is available on [crates.io](https://crates.io). So you can simply
add it to your `Cargo.toml` or add it using

```console
$ cargo add roto
```

Now you're all set up to get started!

## Reading the Docs

If you now run

```console
$ cargo doc --open --package roto
```

You will be able to see the documentation for the `roto` crate, which might
be helpful. The documentation is also available at
[docs.rs/roto](https://docs.rs/roto).
