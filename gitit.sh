#!/bin/bash

# Script to make developing the wiki a little easier
# and less error-prone. NOT a substitute for knowing
# what each of these commands does!


###########
# settings
###########

GITITDIR="$(cd `dirname "$0"` && pwd)"
CABALDIR="$GITITDIR/.cabal-sandbox"

# this is needeed because if your distro doesn't give /tmp
# execute permissions you have to compile somewhere else,
# or there will be all sorts of weird errors
CABALTMP="$CABALDIR/tmp"


###################
# support routines
###################

prep_packages() {
  # try to install system dependencies,
  # and warn if it doesn't work
  bins=(ghc cabal dot pdflatex)
  pkgs=(haskell-platform haskell-platform graphviz texlive)
  for n in "${!bins[@]}"; do
    b=${bins[$n]}
    p=${pkgs[$n]}
    if [[ -z "$(which $b)" ]]; then
      [[ -z "$(which apt-get)" ]] || sudo apt-get install $p
      if [[ -z "$(which $b)" ]]; then
        read -p "WARNING! Couldn't find system binary '$b'. Continue anyway? (y/n) " a
        case "$a" in
          y) continue ;;
          *) exit 1   ;;
        esac
      else
        echo "found system binary $b"
      fi
    else
      echo "found system binary $b"
    fi
  done
}

prep_repo() {
  cd "$GITITDIR"
  [[ -d "$CABALDIR" ]] || cabal update
  cabal sandbox init
  [[ -d "$CABALTMP" ]] || mkdir "$CABALTMP"
}

prep_wiki() {
  prep_repo
  cp -r "$GITITDIR/testwiki" "$CABALTMP"
  cp -r "$GITITDIR/plugins"  "$CABALTMP/testwiki"
  cd "$CABALTMP/testwiki/wikidata"
  [[ -d .git ]] || git init
  git add . && git commit -m 'make sure test pages will show up'
}

cabal_flags() {
  # and this is needed because ghc doesn't understand cabal sandboxes yet
  # see http://mappend.net/posts/ghc-and-cabal-sandbox-playing-ni
  prep_repo > /dev/null
  cabal sandbox hc-pkg list | grep \: | tac | sed 's/://' |
    while read db; do
      echo -n "--package-db='$db' "
    done
}

cabal_vars() {
  vars="CABAL_SANDBOX_CONFIG='$GITITDIR/cabal.sandbox.config'"
  vars="$vars PATH='$CABALDIR/bin:$PATH'"
  vars="$vars TMPDIR='$CABALTMP'"
  echo -n $vars
}

cabal_sandbox() {
  # run cabal with environment variables set correctly
  prep_repo
  time eval "$(cabal_vars) cabal $@ $(cabal_flags)"
}


################
# main routines
################

gitit_build() {
  # build gitit, but don't run it yet
  cd "$GITITDIR"
  prep_packages || exit 1
  cabal_sandbox install $@ || return 1

  # TODO there's got to be a better way right?
  #      preferably a cabal option
  chmod +x "$CABALDIR"/share/*-ghc-*/gitit*/plugins/*
}

gitit_rebuild() {
  # delete the sandbox and run build again
  cd "$GITITDIR"
  rm -rf .cabal-sandbox cabal.sandbox.config
  gitit_build $@ || return 1
}

gitit_serve() {
  # run the test wiki using cabal exec
  cmd='gitit +RTS -IO -RTS --config-file testwiki.conf'
  gitit_build || return 1
  prep_wiki
  cmd="'cd '$CABALTMP/testwiki' && $cmd'"
  eval "echo $cmd $@ | $(cabal_vars) cabal exec bash"
}

gitit_repl() {
  # load gitit in a cabal repl
  gitit_build || return 1
  cabal repl $@
}

main="$1"; shift
case "$main" in
  'build'  ) gitit_build   $@ ;;
  'rebuild') gitit_rebuild $@ ;;
  'repl'   ) gitit_repl    $@ ;;
  'serve'  ) gitit_serve    $@ ;;
  *) echo "$0 doesn't handle '$arg'" && exit 1 ;;
esac
