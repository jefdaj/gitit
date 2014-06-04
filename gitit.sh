#!/bin/bash

# Script to make developing the wiki a little easier
# and less error-prone. NOT a substitute for knowing
# what each of these commands does!


###########
# settings
###########

ROOTDIR="$(cd `dirname "$0"` && pwd)"
BINDIR="$ROOTDIR/.cabal-sandbox/bin"

# this is needeed because if your distro doesn't give /tmp
# execute permissions you have to compile somewhere else,
# or there will be all sorts of weird errors
CABALTMP="$ROOTDIR/.cabal-sandbox/tmp"

WIKIDIR="$CABALTMP/testwiki"


###################
# support routines
###################

check_deps() {
  # stop and warn about missing dependencies
  deps=(bash ghc cabal python pip dot)
  for d in ${deps[@]}; do
    if [[ -z "$(which $d)" ]]; then
      read -p "WARNING! Couldn't find '$d'. Continue anyway? (y/n) " answer
      case "$answer" in
        y|Y) continue ;;
        *  ) exit 1   ;;
      esac
    else
      echo "found $d at $(which $d)"
    fi
  done
}

prep_deps() {
  check_deps || exit 1
  pydeps=(pillow pydot pystache)
  pip install ${pydeps[@]} # sudo? --upgrade?
}

prep_repo() {
  cd "$ROOTDIR"
  #cabal update
  cabal sandbox init
  [[ -d "$CABALTMP" ]] || mkdir "$CABALTMP"
}

prep_wiki() {
  prep_repo
  cp -r "$ROOTDIR/testwiki" "$CABALTMP"
  cp -r "$ROOTDIR/plugins"  "$CABALTMP/testwiki"
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
  vars="CABAL_SANDBOX_CONFIG='$ROOTDIR/cabal.sandbox.config'"
  vars="$vars PATH='$BINDIR:$PATH'"
  vars="$vars TMPDIR='$CABALTMP'"
  echo -n $vars
}

cabal_sandbox() {
  # run cabal with environment variables set correctly
  prep_repo
  eval "$(cabal_vars) cabal $@ $(cabal_flags)"
}


################
# main routines
################

gitit_build() {
  # build gitit, but don't run it yet
  cd "$ROOTDIR"
  prep_deps || exit 1
  cabal_sandbox install $@
}

gitit_rebuild() {
  # delete the sandbox and run build again
  cd "$ROOTDIR"
  rm -rf .cabal-sandbox cabal.sandbox.config
  gitit_build $@
}

gitit_exec() {
  # run the test wiki using cabal exec
  gitit_build
  prep_wiki
  cmd="'cd '$CABALTMP/testwiki' && gitit --config-file testwiki.conf'"
  eval "echo $cmd $@ | $(cabal_vars) cabal exec bash"
}

gitit_repl() {
  # load gitit in a cabal repl
  gitit_build
  cabal repl $@
}

dispatch="$1"; shift
case "$dispatch" in
  'build'  ) gitit_build   $@ ;;
  'rebuild') gitit_rebuild $@ ;;
  'exec'   ) gitit_exec    $@ ;;
  'repl'   ) gitit_repl    $@ ;;
  *) echo "$0 doesn't handle '$arg'" && exit 1 ;;
esac
