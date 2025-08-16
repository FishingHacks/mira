# Example config file

# Allowed Values: "20.1", "19.1", "18.0"
LLVM_VERSION="20.1"
# The path to your local llvm build directory (the thing containing bin/llvm-config and lib/libLLVM<something>.a)
# LLVM_PREFIX=
# Use the mold linker instead of whatever rust chooses (https://github.com/rui314/mold)
USE_MOLD=0

# use mold in release
if [[ -v RELEASE ]]; then USE_MOLD=1; fi
