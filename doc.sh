#!/usr/bin/env bash

# generate clap alone -- bunching clap in with other dependencies for some reason stops cargo from being able to find it.
cargo doc --no-deps -p clap
cargo doc --open --no-deps \
 -p mira-argparse \
 -p mira-common \
 -p mira-doc \
 -p mira-driver \
 -p mira-error-codes \
 -p mira-errors \
 -p mira-lexer \
 -p mira-linking \
 -p mira-llvm-backend \
 -p mira-macros \
 -p mira-parser \
 -p mira-progress-bar \
 -p mira-spans \
 -p mira-target \
 -p mira-typeck \
 -p parking_lot \
 -p inkwell \
 -p cfg-if \
 -p crossbeam-channel \
 -p syn \
 -p proc-macro2 \
 -p quote \
 -p termsize
