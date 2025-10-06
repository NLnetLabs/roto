# Changelog

## 0.8.0

Released 2025-10-06.

### Language

#### Bug fixes

- The trailing comma of match arms is now optional, as it was supposed to be. (#253)

- Fix an issue where the types of float literals weren't properly resolved. (#258)

### Crate

#### Breaking changes

- The functions `SourceFile::read`, `FileTree::read`, `FileTree::single_file`
  and `FileTree::directory` now return `Result` and will no longer panic on
  I/O errors. Thanks @algernon! (#250)

- All custom registered types now _always_ need to wrapped in `Val`. This fixes
  some bugs around registered types and should make it easier to understand when
  `Val` has to be used. Below is a diff illustrating how to migrate. (#247)

```diff
 runtime
-    .register_clone_type::<AddrRange>("A range of IP addresses")
+    .register_clone_type::<Val<AddrRange>>("A range of IP addresses")
     .unwrap();

 // Register the contains method with a docstring
-#[roto_method(runtime, AddrRange)]
+#[roto_method(runtime, Val<AddrRange>)]
 fn contains(range: Val<AddrRange>, addr: Val<IpAddr>) -> bool {
     range.min <= *addr && *addr <= range.max
 }
```

#### New

- The CLI can now run other functions than `main`. Thanks @pieterlexis! (#261)

- It is now possible to register closures. (#248)

- There is an entirely new registration API using the new `library!` macro.
  The previous `Runtime::register_*` methods are all still available, but
  deprecated. (#254 and #262)

```rust
use roto::{Runtime, Val, library};

let lib = library! {
     /// A vector with `x`, `y` and `z` components
     #[clone] type Vec3 = Val<Vec3>;

    impl Val<Vec3> {
        /// Scale this `Vec3` by `r` and return the result
        fn scale(self, r: f32) -> Self {
            Val(self.0 * r)
        }
    }

    /// A vector with all zeroes.
    const ZERO: Val<Vec3> = Val(Vec3 { x: 0.0, y: 0.0, z: 0.0 });
};

let rt = Runtime::from_lib(lib).unwrap();
```

### Documentation

#### New

- The generated documentation is now spread out over multiple pages. It
  generates a page per module and per type. (#254)
- There is a new "Syntax Overview" page in the documentation. (#263)
- Added the following concepts to the language reference: identifiers, booleans,
  `match`, local variables and string formatting. (#263)

#### Bug fixes

- Fixed some examples so that they are consistent with the text and ensure that
  they compile. Thanks @pieterlexis! (#260)
- Fixed many spelling errors in the manual, API docs and the rest of the
  codebase. Thanks @jsoref! (#264)

## 0.7.1

Released 2025-09-26.

## Language

### Bug fixes

- `{{` and `}}` are now unescaped in f-strings. (#257)
- `\u{xxxx}` and `\U{xxxx}` are now properly parsed in f-strings. (#257)
- Fix an issue where function calls could not be used directly in f-strings. (#257)

## 0.7.0

Released 2025-08-25.

This release features a new documentation website available at
<https://roto.docs.nlnetlabs.nl>. If you're new to Roto, it is a great place
to start learning. It is a work in progress, so please give us feedback!

We have also made a tree-sitter grammar for Roto, that is now good enough to
try out: <https://github.com/NLnetLabs/tree-sitter-roto>.

### Language

#### Breaking changes

- The `function` keyword was changed into the `fn` keyword. (#213)
- The `Optional` type has been renamed to `Option`. (#216)
- The `Unit` type has been renamed to `()`. It can also be constructed with
  `()` (#225)

#### New

- Roto now allows multiple items to be imported on a single line. Thanks
  @Wrimo! (#180)

```roto
import foo.{bar1, bar2};
import foo.{bar.fn1, bar2.{fn2, fn3}};
```

- Variants, methods and static methods can now be imported directly. (#202)

```roto
# Import variants
import Option.{Some, None};

# Import a method (as `append`)
import String.append;

# Import a static method (as `new`)
import Prefix.new;

fn foo() {
    let x = Some(10);
    let y = append("race", "car");
    let z = new(1.1.0.0, 16);
}
```

- Methods can now be called as if they were static functions. (#202)

```roto
fn foo() {
    # previously:
    let x = "race".append("car");

    # now also possible:
    let y = String.append("race", "car");
}
```

- Roto now has its first looping construct: `while`. (#194)

```roto
fn factorial(x: u64) -> u64 {
    let i = 1;
    let n = 1;
    while i <= x {
        n = n * i;
        i = i + 1;
    }
    n
}
```

- A new `?` operator, will return a `Option.None` value or yield the value
  inside `Option.Some`, much like the same operator in Rust. (#195)

```roto
fn small(x: i32) -> i32? {
    if x < 10 {
        Some(x)
    } else {
        None
    }
}

fn foo(x: i32, y: i32) -> i32? {
    let z = small(x)? + small(y)?;
    Some(z)
}
```

- The unary minus operator has been added to negate signed integers and
  floating point numbers. (#196)

```roto
fn abs(x: i32) -> i32 {
    if x { x } else { -x }
}
```

- It is now possible to add a type annotation on `let` bindings. (#197)

```roto
fn foo() -> u64 {
    let x: u64 = 10;
    x
}
```

- Python-like f-strings have been added. This is a basic implementation
  that is still rough around the edges. All primitives and registered types
  with a `to_string` method can be put in a string prefixed with `f`. Any
  expression evaluating to such a type is valid. See issue #244 for the
  current limitations. (#220)

```roto
fn foo() -> String {
    let x: u64 = 10;
    f"The value of x is {x} and twice x is {2 * x}"
}
```

#### Bug fixes

- Roto now has an additional compiler pass: MIR. A lot of the code generation
  was rewritten from scratch for this change. With the MIR we model values
  passed to Roto in a more robust and several memory safety issues were fixed
  in the process. (#182)

- The error message for multiple item declarations with the same name have been
  improved, especially when two tests with the same name are declared. (#191)

- The error messages for unresolved paths have been improved. (#198)

- String literals are now properly unescaped. (#200)

- The previous implementation of constants turned out to be unsound, so a new
  implementation should fix memory safety issues around them. (#208)

- The `Reflect` and `RotoFunc` traits are now sealed because they should not
  be implemented by downstream crates. (#214)

- Fixed a bug where it wasn't possible to use multiple `not` operators. (#195)

- Fixed an incorrect error message for `==` that showed the wrong type as
  expected. (#242)

### Crate

#### Breaking changes

- The CLI is now gated by a `cli` feature flag. (#175)
- Logging is now gated by a `logger` feature flag. (#175)
- The fields of `Runtime` are now private. (#193)
- `Runtime` is passed by reference instead of moved to the compilation procedure,
  allow for easier reuse across multiple compilations. (#193)
- Registered constants are now required to be `Send + Sync`. (#208)
- Registered constants are now required to have a valid identifier as name. (#223)

#### New

- There is a new `Compiled::get_tests` method to get the tests in a Roto script
  without running them. This allows more flexibility in how the tests are run
  and what the output is. Thanks @algernon! (#185)
- The `Runtime::add_io_functions` method adds a `print` method to the runtime,
  making it possible to write `"Hello, world!"` to the terminal! (#193)
- `Runtime::compile` is a convenience function that will compile the script at
  the given path. It is no longer necessary to make a `FileTree` first. (#193)

#### Bug fixes

- Fixed some cases where it was impossible to return certain types from
  registered functions. (#210)

#### Other changes

- We switched from the `icu` crate to `unicode-ident` for the parsing of
  identifiers. This shrinks the dependency graph of `roto`. (#173)
- We removed unnecessary files from the crate as published on <crates.io>.
  (#174)
- `Runtime` has improved documentation that was previously on the non-public
  `runtime` module. (#193)

## 0.6.0

Released 2025-06-10.

### Breaking changes

- The minimum supported Rust version is now 1.84.
- The `Compiled::get_function` method no longer takes separate type parameters
  for the parameter types and the return type, but a single function pointer
  type. (#163)

```rust
// old
compiled.get_function::<Ctx, (bool, i32), Val<Verdict>>("main");

// new
compiled.get_function::<Ctx, fn(bool, i32) -> Val<Verdict>>("main");
```

- Registered functions no longer take arguments of runtime types by reference.
  Instead they take `Val<T>`, mirroring how they are passed in from Rust to Roto.
  (#138)

```rust
// old
#[roto_function(runtime)]
fn foo(x: &Foo) -> Foo {
    todo!()
}

// new
#[roto_function(runtime)]
fn foo(x: Val<Foo>) -> Val<Foo> {
    todo!()
}
```

- The return type of registered functions now needs to implement the `Reflect`
  trait. (#163)

### New

- Roto now features an assignment operator to update the value of a variable
  or a field. (#158)
- The CLI now has a subcommand `print` to print out a Roto file with basic
  syntax highlighting. (#139)
- The return type of registered functions can now be `Optional` or a `Verdict`.
  (#163)
- The `FileSpec` type is now public in the `roto` crate. This allows users to
  set up their own systems for script discovery and easier testing. (#151)
- Roto will now give better error messages when keywords are used as
  identifiers. (#157)

### Bug fixes

- Fixed the implementation of the `==` and `!=` operators for `String`. (#141)
- Fixed a couple of double frees and memory leaks. We now run the entire test
  suite under Valgrind to ensure that we will find these issues sooner.
  (#147, #149, #158)
- Fixed an issue with the type inference of the return types of `filtermaps`.
  (#152)

### Other changes

- Several outdated examples have been removed and examples of several errors
  have been updated. (#164)
- We now do snapshot testing of error messages emitted by Roto. (#165)

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
