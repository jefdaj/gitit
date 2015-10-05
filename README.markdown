feat/nix-build
==============

Build files for the Nix package manager.
Rebase from the latest [upstream release](https://github.com/jgm/gitit/releases).

Use `nix-build` to build the gitit binaries, or `nix-shell`
to enter a shell with preinstalled dependencies suitable for hacking.
Note that the shell doesn't include its own cabal-install.
To build inside the shell, do:

    nix-shell
    cabal update
    cabal configure
    cabal build

Or you can substitute `cabal repl` at the end.
