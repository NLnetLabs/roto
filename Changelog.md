# Changelog

## Unreleased new version

Release yyyy-mm-dd.

### Breaking changes

### New

### Bug fixes

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
