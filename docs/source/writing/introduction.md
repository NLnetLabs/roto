# Introduction

This chapter will introduce Roto and guide you towards being able to write
your own scripts with it. This is not meant to be a comprehensive reference.
If you want to know _everything_ that Roto has to offer, look at the
[](lang), instead.

While you're learning the language, there are two pages you might
want to keep open as a reference, so you can look up anything you don't
understand:

 - [](syntax_overview)
 - [](lang)

The easiest way to follow along is to [install the standalone
compiler](installation) and run the examples locally. You can run a script with
the following command:

```console
$ roto run path-to-script.roto
```

When a Roto script is run, it is first compiled, and any syntax or type
checking errors will be reported. Then the function called `main` will
be called. The minimal script you need is therefore the following:

```roto
fn main() {
    # your code goes here
}
```

Every statement you write in Roto must end with a semicolon. You can
add [comments](lang_comments) to your code with `#`, which will make Roto
ignore the rest of the line.

Let's get started!
