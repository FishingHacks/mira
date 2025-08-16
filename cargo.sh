#!/bin/sh

if [[ $# < 1 ]]; then
    cargo --help
    exit 0
fi

set -euo pipefail

# Set `$RELEASE` if this is a release build for the config to properly react to that
for var in "$@"; do
    if [[ "$var" == "--" ]]; then break; fi
    if [[ "$var" == "--release" ]]; then
        export RELEASE=1
        break
    fi
done

ENV_BEFORE="$(printenv)"

# Source either the local config (`./config.sh`), the example config (`./config.example.sh`), or error that no config was found.
if [[ -e ./config.sh ]]; then source ./config.sh
elif [[ -e ./config.example.sh ]]; then source ./config.example.sh
else
    echo "no config (config.example.sh or config.sh) were found"
    exit -1
fi

eval "$ENV_BEFORE"

if [[ -v RELEASE ]]; then unset RELEASE; fi

# set the correct feature flag, as well as the proper
# 'LLVM_SYS_<ver>_PREFIX' env var for the selected llvm version.
EXTRA_ARGS=("-F")
case "${LLVM_VERSION:-}" in
    "20.1")
        EXTRA_ARGS+="llvm20-1"
        if [[ -n "${LLVM_PREFIX:-}" ]]; then export LLVM_SYS_201_PREFIX="$LLVM_PREFIX"; fi
    ;;
    "19.1")
        EXTRA_ARGS+="llvm19-1"
        if [[ -n "${LLVM_PREFIX:-}" ]]; then export LLVM_SYS_191_PREFIX="$LLVM_PREFIX"; fi
    ;;
    "18.0")
        EXTRA_ARGS+="llvm18-0"
        if [[ -n "${LLVM_PREFIX:-}" ]]; then export LLVM_SYS_180_PREFIX="$LLVM_PREFIX"; fi
    ;;
    *)
        echo "Invalid llvm version '$LLVM_VERSION'"
        exit -1
    ;;
esac

export MIRAC_COMMIT_HASH="$(git rev-parse HEAD)"
export MIRAC_COMMIT_HASH_SHORT="$(git rev-parse --short HEAD)"
export MIRAC_COMMIT_DATE="$(git --no-pager log -1 --format='%cs')"

if [[ "${USE_MOLD:-0}" == "1" ]]; then
    mold -run cargo $1 --no-default-features "${EXTRA_ARGS[@]}" "${@:2}"
else
    cargo $1 --no-default-features "${EXTRA_ARGS[@]}" "${@:2}"
fi
