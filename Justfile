alias t := test
alias nt := nextest
alias tv := test-verbose
alias b := build
alias c := clippy
alias v := valgrind

default:
    @just --list

run CMD SCRIPT:
    cargo run -- '{{CMD}}' '{{SCRIPT}}'

build:
    cargo build

test:
    cargo test

test-verbose TEST="":
    RUST_LOG=info cargo nextest run --no-capture '{{TEST}}'

nextest TEST="":
    cargo nextest run

clippy:
    cargo clippy -all

valgrind TEST="":
    VALGRINDFLAGS="--suppressions=valgrind_suppressions.supp" cargo valgrind test -- {{TEST}}
