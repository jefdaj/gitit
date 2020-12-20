Gitit
=====

My fork of [gitit](https://github.com/jgm/gitit), specialized for use as a lab notebook.

The master branch is messy and consists of the upstream gitit release (v0.13.0)
+ all my working feature branches. Stable versions are tagged. Here's a summary
of the other feature branches, which are each rebased separate from upstream:

| feature          | status      | description                                            |
|:-----------------|:------------|:-------------------------------------------------------|
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
