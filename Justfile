alias t := test
alias nt := nextest
alias tv := test-verbose
alias b := build
alias c := clippy
alias v := valgrind

default:
    @just --list

ci:
    @just test
    @just clippy
    @just fmt-check
    @just valgrind

run CMD SCRIPT:
    cargo run -- '{{CMD}}' '{{SCRIPT}}'

build:
    cargo build

test:
    cargo test

fmt-check:
    cargo fmt --all --check

fmt:
    cargo fmt --all

test-verbose TEST="":
    RUST_LOG=info cargo nextest run --no-capture '{{TEST}}'

nextest TEST="":
    cargo nextest run

clippy:
    cargo clippy --all

valgrind TEST="":
    # We ignore doctests by passing --all-targets, because apparently doctests leak memory
    # sometimes, randomly, unexpectedly and frustratingly.
    VALGRINDFLAGS="--suppressions=valgrind_suppressions.supp" cargo valgrind test --all-targets -- {{TEST}}
