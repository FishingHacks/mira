# Mira - The new (really shitty) programming language on the block

Mira is a general purpose programming language designed to make interoperability with c very easy through features such as bindgen at compiletime.

> Note however that it is *still* work-in-progress and a lot of features such as closures, generics, c bindgen, ranges, for loops, iterators and others don't work yet.

# Documentation

You can look at the [Language Reference](./langref.md), the [standard library documentation](./std_docs.md).

# Installation

- [You can download one of the prebuilt mira binaries](https://github.com/fishingHacks/mira/releases)

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
- LLVM development libraries == 19.1.x (Other versions might work. You can change the llvm feature of the `inkwell` dependency in ./mira/Cargo.toml file)
- Cargo

In case you have the llvm binaries not exposed, you can set the environment variable `LLVM_SYS_170_PREFIX` to the build directory of llvm (the one where bin/llc or bin/opt is in).

Then building mirac is pretty easy:

```
$ cargo build           // <- for debug mode (you probably don't want that)
$ cargo build --release // <- for release mode, allows for more aggressive optimizations. This is recommended and the mode the prebuilt binaries are built with
```

# Syntax Highlighting

You can use [tree-sitter-mira](htps://github.com/fishinghacks/tree-sitter-mira) for syntax highlighting in editor that support it. Please do note that in neovim this is quite a clunky process. You'll have to clone the repository somewhere, symlink the queries to `~/.config/nvim/queries/mira`, and add the following to ur lua config:

```lua
local parser_config = require('nvim-treesitter.parsers').get_parser_configs()
parser_config.mira = {
  install_info = {
    -- Change this url to your grammar
    url = '<root directory of the cloned repository>',
    -- If you use an external scanner it needs to be included here
    files = { 'src/parser.c', 'src/scanner.c' },
    generate_reqires_npm = false,
    requires_generate_from_grammar = false,
  },
  -- The filetype you want it registered as
  filetype = 'mr',
}
vim.treesitter.language.register('mira', 'mira')
vim.filetype.add { extension = { mr = 'mira' } }
```

and then run `:TSInstall mira` or `:TSInstallFromGrammar mira` if :TSInstall fails. You should then be able to just open .mr files and get syntax highlighting.
Note: there might be times where treesitter fails to parse, but it does not seem to effect highlighting at all.
