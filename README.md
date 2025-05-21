`Roto`
======

`Roto` is an embedded scripting language that is fast, safe and easy to use. 

Primarily used by [Rotonda], the composable, programmable routing BGP engine. It
is made to integrate especially well with Rotonda, so that writing filters is as
simple as possible.

Read more about it in the [documentation].

## Example

```roto
# A function that returns true if an IP address is equal to 0.0.0.0
function is_zero(x: IpAddr) -> bool {
    x == 0.0.0.0
}

# A filtermap that only accepts IP addresses of 0.0.0.0
filtermap main(x: IpAddr) {
    if is_zero(x) {
        accept
    } else {
        reject
    }
}
```

More examples can be found in the `examples` folder in this repository. They
can be run with

```
cargo run --example <example name>
```

## Features

- Roto can be **embedded** into any Rust application. Rust types and functions
  can be registered for use in Roto.
- Roto is **strongly and statically-typed**, ensuring that type errors are
  caught at compile-time. This does not mean that the user has to specify types
  everywhere, most types can be inferred by the Roto compiler. When the compiler
  detects a mistake in your script, it will emit a friendly message.
- Scripts are **compiled** to machine code before they are executed. This
  means that they run quickly and introduce minimal latency into your system.
- Roto scripts are **hot-reloadable**. The host application can recompile
  scripts at any time.

## Limitations

- All registered Rust types must implement `Clone` or `Copy`. Rust types that
  don't implement these traits should be wrapped in an `Rc` or `Arc`. The reason
  for this limitation is that Roto does not have references.
- The parameter and return types of functions exported to the host application
  are static.
- Roto currently does not feature any looping constructs. If you need loops,
  you can use recursion instead as a workaround.
- All values are currently immutable. If a type should be mutable and shared
  between multiple variables, it can be wrapped in a type that provides interior
  mutability such as `Rc<RefCell<T>>` and `Arc<Mutex<T>>`.

## Learn more

- Documentation of the Roto language is included in the
  [documentation for Rotonda](documentation).
- The API docs for the latest version are available on [docs.rs]
- Some examples are available in the examples folder of the Roto repository.

## Contributing

If you have comments, proposed changes, or would like to contribute,
please open an issue in the [GitHub repository]. In particular, if you
would like to use the crate but it is missing functionality for your use
case, we would love to hear from you!

[Rotonda]: https://github.com/NlnetLabs/rotonda
[GitHub repository]: https://github.com/NLnetLabs/routecore
[Documentation]: https://rotonda.docs.nlnetlabs.nl/en/latest/roto/00_introduction.html
[crate]: https://crates.io/crates/roto
[docs.rs]: https://docs.rs/roto

## License

The _roto_ crate is distributed under the terms of the BSD-3-clause license.
See LICENSE for details.
