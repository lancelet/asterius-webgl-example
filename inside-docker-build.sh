#!/usr/bin/env bash
#
# This should be run from inside terrorjack/asterius image.
mkdir -p asterius-bin
ahc-cabal new-install . -j1 --symlink-bindir asterius-bin
pushd asterius-bin
ahc-dist --browser --input-exe awgl
popd
