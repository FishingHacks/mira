# Mira - The new (really shitty) programming language on the block

Mira is a general purpose programming language designed to make interoperability with c very easy through features such as bindgen at compiletime.

> Note however that it is *still* work-in-progress and a lot of features such as closures, generics, c bindgen, ranges, for loops, iterators and others don't work yet.

# Documentation

You can look at the [Language Reference](./langref.md), the [standard library documentation](./std_docs.md).

# Installation

- [You can download one of the prebuilt mira binaries](github.com/fishingHacks/mira/releases)

A Mira installation is composed of 2 things:

1. The Executable
2. The lib/ directory

At runtime, mira searches up to 10 directories up for `lib/<name>` and `lib<name>`, for example with `mirastd`, it searches:

- `./lib/mirastd`
- `./libmirastd`
- `../lib/mirastd`
- `../libmirastd`
- `../../lib/mirastd`
- `../../libmirastd`
- (and so on)

This allows you to unpack the installation anywhere. This also does support having the standard libraries in /lib or /usr/lib and have mira in /bin or /usr/bin respectively.

# Building from source

Ensure you have the following dependencies:

- System C/C++ Toolchain
- LLVM development libraries == 17.0.x (Note that if you're building llvm, you have to clone llvm/llvm-project, just the llvm subdirectory will not work.)
- Cargo

In case you have the llvm binaries not exposed, you can set the environment variable `LLVM_SYS_170_PREFIX` to the build directory of llvm (the one where bin/llc or bin/opt is in).

Then building mirac is pretty easy:

```
$ cargo build -j8           // <- for debug mode (you probably don't want that)
$ cargo build -j8 --release // <- for release mode, allows for more aggressive optimizations. This is recommended and the mode the prebuilt binaries are built with
```

Note that `-j8` denotes the amount of workers you want to use at the same time, you can increase/decrease it depending on how many threads your cpu has.
