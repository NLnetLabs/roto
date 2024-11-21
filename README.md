`Roto`
======

`Roto` is a full programming language that is fast, safe and easy to use. It
is made to integrate especially well with Rotonda, so that writing filters is
as simple as possible.

Used by [Rotonda], the composable, programmable routing BGP engine.

Scripts are compiled to machine code by Rotonda before they are executed. This
means that they run quickly and introduce minimal latency into your system.

A strong and static type system ensures that every expression must be of a
well defined, unambiguous type. Roto scripts therefore cannot crash Rotonda
and can be used safely. This does not mean that the user has to specify types
everywhere, most types can be inferred by the Roto compiler. When the compiler
detects a mistake in your script, it will emit a friendly message.

Roto has no facilities to create loops. The reason for this is that scripts
need to run only for a short time and should not slow down the application.

Read more about in the [documentation].

## Contributing

If you have comments, proposed changes, or would like to contribute,
please open an issue in the [GitHub repository]. In particular, if you
would like to use the crate but it is missing functionality for your use
case, we would love to hear from you!

[Rotonda]: https://github.com/NlnetLabs/rotonda
[GitHub repository]: https://github.com/NLnetLabs/routecore
[Documentation]: https://rotonda.docs.nlnetlabs.nl
[crate]: https://crates.io/crates/roto

## License

The _roto_ crate is distributed under the terms of the BSD-3-clause license.
See LICENSE for details.
