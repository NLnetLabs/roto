# Roto Fuzzers

The fuzzers for Roto use `cargo-fuzz`. You'll need that to run the fuzz tests:

```console
$ cargo install cargo-fuzz
```

Then to run a fuzzing target:

```console
$ cargo +nightly fuzz run lexer
```

Note that the fuzzer only works with nightly Rust.

You can get a full list of fuzzing targets with:

```console
$ cargo +nightly fuzz list
```
