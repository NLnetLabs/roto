# Changelog

## 0.4.0

Release 2025-01-29.

### Breaking changes

- Filter maps no longer have a `define` and an `apply` block. `let` bindings
  should be used instead. For example:

```roto
# Roto 0.3
filter-map foo() {
    define {
        x = 4;
    }
    apply {
        accept x;
    }
}

# Roto 0.4
filter-map foo() {
    let x = 4;
    accept x;
}
```

### New

- Variables can now be created in more places with `let` bindings. `let`
  bindings replace the `define`/`apply` mechanism and are much more general
  because they can be used in any block. For example, inside functions or
  `if`-`else` blocks. The binding is scoped until the end of the block it is
  defined in. It is not allowed to declare multiple variables with the same
  name within a single block.
- The `String` type was added to represent pieces of text. String literals
  are enclosed in double quotes (`"`).
- It is now possible to declare constants in the runtime for a Roto script.
- Similarly, it is now possible to add "context" to Roto scripts. The variables
  in the context behave like constants, but they can differ between different
  calls to a script. In other words, constants are defined when the script is
  _compiled_, context is defined when the script is _run_.

### Bug fixes

- 64-bit numbers (i.e. `i64` and `u64`) were not type checked and compiled
  properly. They are now fixed.

## 0.3.0

Released 2024-11-21.

This is a completely rewritten version of Roto. The API and the language
itself are both completely different. See the API docs and the `examples/`
folder to see how to use it.

### Breaking changes

- There is no `VM` anymore.
- A script needs a `Runtime` with Rust objects to reference
- Many syntax changes to the Roto language.
- The `TypeValue` enum no longer exists, instead native Rust types are used
- No macros are used anymore for Roto types, they _can_ still be used for
  functions and methods, but they can also be added programmatically.
- As a corollary, the definitions for Roto types that are specific to
  Rotonda have moved to Rotonda.
- And more changes.

### New

- Types, functions and methods can be registered via `Runtime`.
- `Runtime::print_documentation` prints the documentation for all items
  registered in the Runtime.
- Roto is now compiled using cranelift and runs as native machine code.
- Parsing and type error messages have improved.


## 0.2.0

Released 2024-01-18.

First release.

Other changes

No changes compared to 0.1.0-rc0, but due to an error in the versioning of the
placeholder for this crate (0.1.0), we have to move to 0.2.0.

## 0.1.0-rc0

Released 2024-01-10.

First release candidate.
