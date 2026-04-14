# Why Roto?

If you have an application written in Rust, and you (or anyone else using your application) would like to be able to automate, extend, or customise the application without having to recompile it, an embedded scripting language might be a good choice. Common scripting languages include JavaScript and Lua, however, these are cumbersome to integrate, impose significant overhead, and are not a good match for Rust's design. Roto is a lightweight scripting language designed to be easy to integrate with Rust, and provides a simple and intuitive API for calling Rust code from Roto, and vice versa. Roto also inherits many of Rust's own features, including strong static typing, making it safer than other languages.

Roto provides high performance through its use of the Cranelift JIT compiler, hot reloading (so you don't need to restart your application), and is easy to learn and debug.

Roto was primarily created to support [Rotonda](https://nlnetlabs.nl/projects/routing/rotonda/), a BGP routing engine written in Rust, where it is used to write filters for routing decisions, but it is a general-purpose scripting language that can be embedded into any Rust application.
