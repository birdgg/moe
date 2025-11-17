#!/bin/bash
set -euo pipefail

apk update && apk add --no-cache binutils file

# we mounted the source repo as a Docker volume in /mnt
cd /mnt


######## Cabal build

# static link binary
cabal update
cabal build exe:moe --enable-executable-static


##########

# copy binary
mkdir -p out/
BIN_PATH="$(cabal -v0 list-bin exe:moe)"
cp "${BIN_PATH}" out/moe

# strip linker symbols
strip out/moe

ls -lsa out/

# look for "statically linked" in the output of 'file':
file out/moe
