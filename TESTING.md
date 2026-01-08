# Testing

The tests of Roto live in a few places:

 - unit tests in the source code
 - doctests in the source code
 - UI tests in `tests/`

Running these tests is done with a single command:
```
cargo test
# or with nextest:
cargo nextest run
```

## Snapshot testing

The UI tests use snapshot testing using [insta]. You can run
them with the regular Rust test runner, but to change them, you might want to
install `cargo-insta` with `cargo install cargo-insta`. You can add tests by
putting them in `tests/scripts/type_errors` and `tests/scripts/parse_errors`.
Then you can run `cargo test` followed by `cargo insta review` to review the new
tests and accept their output. See the [insta] website for more information.

## Valgrind

We require that the entire test suite is run under [Valgrind]. To do that locally,
you'll need to install Valgrind and `cargo-valgrind`.

If you have [just] installed, you can run the test suite under valgrind with

```
just valgrind
```

But you can also use the full expression without [just] if you prefer:

```
VALGRINDFLAGS="--suppressions=valgrind_suppressions.supp" cargo valgrind test --all-targets
```

## MIRI

A subset of tests is also run under MIRI, which is even more strict than
Valgrind, but also cannot run all tests.

You can run the relevant tests under MIRI with:

```
MIRIFLAGS=-Zmiri-disable-isolation cargo +nightly miri test
```

[insta]: https://insta.rs/
[Valgrind]: https://valgrind.org/
[just]: https://just.systems/man/en/
