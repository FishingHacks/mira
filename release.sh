#!/usr/bin/env bash

./cargo.sh build --release $@
cargo build --release -p mira-doc $@

if [[ -e ./release ]]; then
    printf "directory 'release' already exists. do you want to delete it? [Y/n] "
    while read RES; do
        if [[ "$RES" == "y" || "$RES" == "" ]]; then break; fi
        if [[ "$RES" == "n" ]]; then exit 0; fi
        printf "directory 'release' already exists. do you want to delete it? [Y/n] "
    done
    echo "deleting ./release/"
    rm -r ./release/
fi
echo "creating mira dir"
mkdir -p release/mira
cd release/mira
echo "copying mirac"
cp ../../target/release/mirac .
cp ../../target/release/mira-doc .
echo "copying libs"
cp -r ../../lib .
cd ..
echo "compressing"
tar czf ./mira-x86_64.tar.gz ./mira
echo "done"
