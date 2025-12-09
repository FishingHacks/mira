# Mira - The new (really shitty) programming language on the block

Mira is a general purpose programming language designed to make interoperability with c very easy through features such as bindgen at compiletime.

> Note however that it is *still* work-in-progress and a lot of features such as closures, generics, c bindgen, ranges, for loops, iterators and others don't work yet.
>
> Note 2 electric boogaloo: This is still an experimental compiler and breaking changes whenever yippieeee

# Documentation

You can look at the [Language Reference](./langref.md).

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
- LLVM development libraries (either 18.0, 19.1 or 20.1, configurable in [the local config](./config.sh) or [the example config](./config.example.sh) if you're using cargo.sh or by specifying `--no-default-features -F llvm<version>` as arguments during the `cargo` invocation. you may also need to set the llvm prefix, doable by setting `LLVM_PREFIX` in either of the cargo.sh configs or `LLVM_SYS_180_PREFIX`, `LLVM_SYS_191_PREFIX`, or `LLVM_SYS_201_PREFIX` environment variable depending on the llvm version you're using.)
- Cargo

Then run `cargo build`, or if you're using cargo.sh to have config.example.sh or config.sh configuration applied, use `./cargo.sh build`

If you are interested in a release build, use either `cargo build --release` or `./cargo.sh build --release`, or use `./release.sh`, which will invoke cargo.sh and put all the required files for a mira installation into `./release/mira` and compress them into `./release/mira-x86_64.tar.gz`.

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
