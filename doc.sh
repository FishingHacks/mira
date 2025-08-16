#!/bin/sh

# generate clap alone -- bunching clap in with other dependencies for some reason stops cargo from being able to find it.
cargo doc --no-deps -p clap
cargo doc --open --no-deps \
 -p mira \
 -p mira-driver \
 -p mira-error-codes \
 -p mira-errors \
 -p mira-lexer \
 -p mira-macros \
 -p mira-progress-bar \
 -p mira-spans \
 -p mira-parser \
 -p mira-common \
 -p parking_lot \
 -p inkwell \
 -p termsize \
 -p crossbeam-channel \
 -p syn \
 -p proc-macro2 \
 -p quote \
 -p cfg-if

