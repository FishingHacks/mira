#!/bin/sh

# generate clap alone -- bunching clap in with other dependencies for some reason stops cargo from being able to find it.
cargo doc --no-deps -p clap
cargo doc --open --no-deps \
 -p mira \
 -p mira-errors \
 -p mira-error-codes \
 -p parking_lot \
 -p thiserror \
 -p inkwell \
 -p termsize \
 -p crossbeam \
 -p cfg-if
