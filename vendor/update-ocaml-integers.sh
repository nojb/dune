#!/bin/bash

version=0.5.1

set -e -u -o pipefail

TMP="$(mktemp -d)"
TRAP_CMD=$(printf "rm -rf %q" "$TMP")
trap "$TRAP_CMD" EXIT

lib_name=ocaml-integers

rm -rf "$lib_name"
mkdir -p "$lib_name/src"

(
    cd "$TMP"
    git clone https://github.com/ocamllabs/$lib_name.git "$lib_name"
    cd "$lib_name"
    git -c advice.detachedHead=false checkout "$version"
)

SRC=$TMP/$lib_name

cp -v "$SRC"/src/*.{ml,mli,c,h} "$lib_name"/src
cp -v "$SRC"/src/dune "$lib_name"/src
git checkout opam-file-format/src/dune

git add -A .
