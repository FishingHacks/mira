#!/usr/bin/env bash

# generate clap alone -- bunching clap in with other dependencies for some reason stops cargo from being able to find it.
cargo doc --no-deps -p clap
cargo doc --open --no-deps \
 -p mira-argparse \
 -p mira-typeck \
 -p mira-error-codes \
 -p mira-driver \
 -p mira-target \
 -p mira-linking \
 -p mira-parser \
 -p mira-common \
 -p mira-errors \
 -p mira-lexer \
 -p mira-macros \
 -p mira-llvm-backend \
 -p mira-progress-bar \
 -p mira-spans \
 -p parking_lot \
 -p inkwell \
 -p cfg-if \
 -p crossbeam-channel \
 -p syn \
 -p proc-macro2 \
 -p quote \
 -p termsize
