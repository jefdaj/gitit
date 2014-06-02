#!/bin/bash

# Script to make developing the wiki a little easier
# and less error-prone. NOT a substitute for knowing
# what each of these commands does!

ROOTDIR="$(cd `dirname "$0"` && pwd)"
PATH="$ROOTDIR/.cabal-sandbox/bin:$PATH"
TMPDIR="$ROOTDIR/.cabal-sandbox/tmp"

prepare_repo() {
  cd "$ROOTDIR"
  cabal update
  cabal sandbox init
  [[ -d "$TMPDIR" ]] || mkdir "$TMPDIR"
}

build_gitit() {
  cd "$ROOTDIR"
  cabal install
}

test_gitit() {
  cd "$ROOTDIR/testwiki/wikidata"
  [[ -d .git ]] || git init
  git add . && git commit -m 'make sure test pages will show up'
  cd "$ROOTDIR/testwiki"
  gitit --config-file testwiki.conf
}

set -e
for arg in "$@"; do
  case "$arg" in
    'prep' ) prepare_repo ;;
    'build') build_gitit  ;;
    'test' ) test_gitit   ;;
    *) echo "unexpected arg: '$arg'" && exit 1 ;;
  esac
done
