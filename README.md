Roto
====

[![CI](https://github.com/NLnetLabs/roto/actions/workflows/ci.yml/badge.svg?branch=main&event=push)](https://github.com/NLnetLabs/roto/actions?query=workflow%3Aci)
[![docs.rs](https://img.shields.io/docsrs/roto?label=docs.rs)](https://docs.rs/roto)
[![Documentation Status](https://app.readthedocs.org/projects/roto/badge/?version=latest)](https://roto.docs.nlnetlabs.nl/)
[![Discuss on Discourse](https://img.shields.io/badge/Discourse-NLnet_Labs-orange?logo=Discourse)](https://community.nlnetlabs.nl/)
[![Mastodon Follow](https://img.shields.io/mastodon/follow/114692612288811644?domain=social.nlnetlabs.nl&style=social)](https://social.nlnetlabs.nl/@nlnetlabs)

Roto is an embedded scripting language for Rust applications that is fast, safe
and easy to use. 

The language is primarily used by [Rotonda], the composable, programmable
routing BGP engine. It is made to integrate especially well with Rotonda, so
that writing filters is as simple as possible. In addition, Roto can be easily
embedded into any Rust application for general purpose scripting.

Read more about it in the [documentation].

## Example

```roto
# A function that returns true if an IP address is equal to 0.0.0.0
fn is_zero(x: IpAddr) -> bool {
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

```sh
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

These limitations are fundamental to the design of Roto. They stem from the
fact that Roto is a scripting language and that Rust's reflection system is
limited.

- All registered Rust types must implement `Clone` or `Copy`. Rust types that
  don't implement these traits should be wrapped in an `Rc` or `Arc`. The reason
  for this limitation is that Roto does not have references and freely clones
  values.
- It is not possible to register types that are not concrete. For example,
  `Vec<u32>` is possible, but `Vec<T>` is not. We plan to support registering
  generic via some form of type erasure.
- The parameter and return types of functions exported to the host application
  must have a `'static` lifetime.

## Pending features

Some limitations are only present because we haven't come around to
implementing them yet. Most limitations can be found in the issue tracker, but
we've summarized some important missing features here.

- It's not yet possible to write generic functions.
  ([#190](https://github.com/NLnetLabs/roto/issues/190))
- It's not yet possible to create global constants.
  ([#344](https://github.com/NLnetLabs/roto/issues/344))
- Functions are not yet first-class, meaning that they cannot be passed around
  as values. ([#122](https://github.com/NLnetLabs/roto/issues/122))
- Roto does not have maps/dictionaries yet.
  ([#345](https://github.com/NLnetLabs/roto/issues/345))

## Memory safety

Roto fundamentally relies on unsafe code, after all, we are generating machine
code at runtime. However, we treat every unsoundness stemming from use of Roto
with safe Rust as a bug of high priority. Please report any issues you find to
the [GitHub repository].

We run our extensive test suite under Valgrind in CI to ensure that at least
most common use cases are correctly implemented.

## Security considerations

If you allow users to submit **untrusted** Roto scripts to your application,
you need to be aware that malicious (or erroneous) Roto scripts can do the
following:

- crash your process by running out of memory with infinite recursion,
- loop indefinitely with a `while` loop, or
- be so big that compiling it will slow down your application.

Therefore, we make the following recommendations:

- Impose a maximum size on scripts.
- Compile and run the untrusted script in a separate process with a timeout and
  proper handling of unexpected crashes of that process.

Finally, Roto scripts have access to all functions you provide and are therefore
as contained as you want them to be. Be careful not to expose information or
functionality that compromises the security of your application.

## Learn more

- To learn how to use and embed Roto, you can read the [documentation].
- The API docs for the latest version are available on [docs.rs].
- If you have questions, you can ask them on the [community forum].
- Some examples are available in the `examples/` folder of the Roto repository.

## Contributing

If you have comments, proposed changes, or would like to contribute,
please open an thread on the [community forum]. In particular, if you
would like to use the crate but it is missing functionality for your use
case, we would love to hear from you!

See [CONTRIBUTING.md] for more information.

## License

Roto is distributed under the terms of the BSD-3-clause license.
See LICENSE for details.

[Rotonda]: https://github.com/NlnetLabs/rotonda
[GitHub repository]: https://github.com/NLnetLabs/routecore
[Documentation]: https://roto.docs.nlnetlabs.nl/
[crate]: https://crates.io/crates/roto
[docs.rs]: https://docs.rs/roto
[community forum]: https://community.nlnetlabs.nl/c/roto/7
[CONTRIBUTING.md]: ./CONTRIBUTING.md
