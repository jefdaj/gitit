feat/nix-build
==============

Rebase from the latest release of `jgm/gitit`.

Build files for the Nix package manager.
Use `nix-build` to build the gitit binaries, or `nix-shell`
to enter a shell with preinstalled dependencies suitable for hacking.
Note that the shell doesn't include its own cabal-install,
but it does have `ghc` and `ghci`.
To build inside the shell, do:

    nix-shell
    cabal update
    cabal configure
    cabal build

Or substitute `cabal repl` for the last one.
