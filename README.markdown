Gitit
=====

My fork of [gitit](https://github.com/jgm/gitit), specialized for use as a lab notebook.

This is the master branch, which is messy and consists of all my changes
jumbled together. They can be found separately in the `feat/*` and `pullreq/*`
branches. Here's a summary:

| feature          | status      | description                                            |
|:-----------------|:------------|:-------------------------------------------------------|
| nix-build        | works       | build with the Nix package manager                     |
| cache-dir        | works       | provide plugins access to the cache                    |
| csv-plugin       | works       | render CSV (inline or from files) as markdown tables   |
| files-plugin     | works       | render clickable lists of files                        |
| nix-build        | works       | build gitit using the Nix package manager              |
| svg-dot-plugin   | works       | improved dot plugin (vector graphics, clickable links) |
| compiled-plugins | works       | use plugins without the overhead of dynamic loading    |
| citations        | in progress | automatic citations from page metadata or a bib file   |
| related-files    | in progress | auto-list files (PDFs etc) related to current page     |

Both [Nix](https://nixos.org/nix) and [Stack](https://www.haskellstack.org/)
builds are working. I use `stack ghci` for fast incremental compilation of the
Haskell code, or `nix-shell` to work on plugins in other languages. Then I
`nix-build` the final package to include the plugins' runtime dependencies.

Compiling
---------

Use `nix-build` to build the gitit binaries, or `nix-shell`
to enter a shell with preinstalled dependencies suitable for hacking.
Note that the shell doesn't include its own cabal-install.
To build inside the shell, do:

    nix-shell
    cabal update
    cabal configure
    cabal build

Or you can substitute `cabal repl` at the end.
