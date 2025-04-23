# Changelog

## Unreleased new version

Released yyyy-mm-dd.

### Breaking changes

### New

### Bug fixes

### Other changes

## 0.5.0

Released 2025-04-23.

### Breaking changes

- Renamed `Runtime::basic()` to `Runtime::new()`.
- Reading Roto scripts should now be done with a `FileTree` instead of
  `roto::read_files`.
- It is no longer necessary to pass the pointer size to Roto.
- The `roto_method`, `roto_static_method` & `roto_function` macros are more
  strict about the types they accept. Previously, some parameters could be
  marked as references where they should have been passed by value and vice
  versa, leading to incorrect behaviour.
- Hyphens are no longer allowed in identifiers (such as names of variables,
  functions, types and filtermaps).
- `filter-map` should now be written as `filtermap`
- Some outdated constructs were removed and are now treated as parse errors:
  `rib`, `table`, `output-stream`.

### New

- There is a full module system for Roto now, meaning that scripts can now be
  spread out over multiple files. More information can be found in the
  documentation.
- Optional types have been added. For any type `T` the type `T?` means an
  optional value of that type, similar to Rust's `Option<T>`. Values of that
  type can be constructed with `Optional.None` and `Optional.Some(v)`. It's
  also possible to match on optional values.

```roto
match optional_u32 {
  Some(x) -> x,
  None -> 0,
}
```

- Roto scripts can now contain tests. These are similar to filtermaps
  where `accept` means a passing test and `reject` a failing one.

```roto
test this_passes {
  accept
}

test this_fails {
  reject
}
```

- The `cli` function in the Roto API can be used to create a simple CLI for
  any runtime, allowing for easier debugging and testing of Roto scripts.
- The floating-point types `f32` and `f64` were added, including basic
  operators and some methods for floating point manipulation.

### Bug fixes

- There are several fixes for cloning and dropping of Rust values in Roto
  scripts. The implementation is much more sound now and should not lead to
  leakage and double frees anymore.
- Fixed an issue where one runtime methods would override another method causing
  the wrong method to be invoked by a script.
- Roto now uses a custom lexer instead of `logos` for more flexibility. This
  fixes some small issues around complex literals.

## 0.4.0

Released 2025-01-29.

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
