#!/bin/bash

# Script to make developing the wiki a little easier
# and less error-prone. NOT a substitute for knowing
# what each of these commands does!


############
# variables
############

ROOTDIR="$(cd `dirname "$0"` && pwd)"
BINDIR="$ROOTDIR/.cabal-sandbox/bin"
WIKIDIR="$ROOTDIR/testwiki"

# this is needeed because if your distro doesn't give /tmp execute permissions
# you have to compile somewhere else, or there will be all sorts of weird errors
CABAL_SANDBOX_TMPDIR="$ROOTDIR/.cabal-sandbox/tmp"

# and this is needed because ghc doesn't understand cabal sandboxes yet
# see http://mappend.net/posts/ghc-and-cabal-sandbox-playing-ni
CABAL_SANDBOX_PKGPATH="$(cabal sandbox hc-pkg list | grep \: | tac | sed 's/://' | paste -d: - -)"


############
# functions
############

prepare_repo() {
  cd "$ROOTDIR"
  #cabal update
  cabal sandbox init
  [[ -d "$CABAL_SANDBOX_TMPDIR" ]] || mkdir "$CABAL_SANDBOX_TMPDIR"
}

build_gitit() {
  cd "$ROOTDIR"
  TMPDIR="$CABAL_SANDBOX_TMPDIR" cabal install
}

test_gitit() {
  cd "$WIKIDIR/wikidata"
  [[ -d .git ]] || git init
  git add . && git commit -m 'make sure test pages will show up'
  cd "$WIKIDIR"
  CABAL_SANDBOX_CONFIG="$ROOTDIR/cabal.sandbox.config" \
    GHC_PACKAGE_PATH="$CABAL_SANDBOX_PKGPATH" \
    PATH="$BINDIR":$PATH \
    gitit --config-file "$WIKIDIR/testwiki.conf"
}


#######
# main
#######

for arg in "$@"; do
  case "$arg" in
    'prep' ) prepare_repo ;;
    'build') build_gitit  ;;
    'test' ) test_gitit   ;;
    *) echo "unexpected arg: '$arg'" && exit 1 ;;
  esac
done
