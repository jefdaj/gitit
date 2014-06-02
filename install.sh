#!/bin/bash

ROOTDIR="$(cd `dirname "$0"` && pwd)"
export TMPDIR="$ROOTDIR/build"
mkdir "$TMPDIR"

cd "$ROOTDIR"
cabal update
cabal sandbox init
cabal install
