#show raw.where(lang: "roto"): set raw(
    syntaxes: "roto.sublime-syntax",
)

= Declarations

```roto
filtermap foo() {}

filtermap foo(x: i32) {}

filter foo() {}

filter foo(x: i32) {}

fn foo() {}

fn foo(x: i32) -> i32 {}

record Foo {}

record Foo {
    x: i32
}

record Foo {
    x: i32,
    y: i32
}

record Foo {
    x: i32,
    y: i32,
}

test foo {}

test foo {
    accept
}

import foo;

import foo.bar;

import foo.{bar, baz};

import foo.{bar.fn1, baz.{fn2, fn3}};
```

= Expressions

```roto
fn foo() {
    ()
}

fn foo() {
    true
}

fn foo() {
    false
}

fn foo() {
	  5
}

fn foo() {
	"hello world!"
}

fn foo() {
	foo.bar
}

fn foo() {
    bar()
}

fn foo() {
    bar(x)
}

fn foo() {
    bar(x,)
}

fn foo() {
    bar(x, y)
}

fn foo() {
    bar.baz()
}

fn foo() {
    bar?
}

fn foo() {
    bar??
}

fn foo() {
	Foo {}
}

fn foo() {
	Foo { a: 5 }
}

fn foo() {
	Foo { a: 5, }
}

fn foo() {
    if true { 0 }
}

fn foo() {
    if true { 0 }
    return
}

fn foo() {
    if true { 0 } else { 1 }
}

fn foo() {
    if if foo { 5 } { 3 } else { 2 }
}

fn foo() {
  if foo { bar } else if baz { quox } else { quaz }
}

fn foo() {
  while 4 == 4 {}
}

fn foo() {
  while true {
      print("yay");
      print("more yay!");
  }
}

fn foo() {
    while true {}
    return
}

fn foo() {
	match 5 {}
    return
}

fn foo() {
    match foo.is_cool() {
        Foo(x) -> {}
        Bar -> x,
    }
}

fn foo() {
    match foo.is_cool() {
        Foo(x,) -> {}
        Bar -> x,
    }
}

fn foo() {
    2 * 4 / 6 + 2 * 4 == 5 * 6
}

fn foo() {
  - 5 - 3
}

fn foo() {
    [1, 2, 3, 4]
}

fn foo() {
    for x in ["hello", "hi"] {
        print(x);
    }
}
```

= Types

```roto
fn foo() -> () {}

fn foo() -> i32 {}

fn foo() -> Bar {}

fn foo() -> Bar[x] {}

fn foo() -> Bar[x,] {}

fn foo() -> Bar[x,y] {}

fn foo() -> { a: i32 } {}

fn foo() -> { a: i32, } {}

fn foo() -> { a: i32, b: bool } {}
```
