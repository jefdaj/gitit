Gitit
=====

This is my fork of [gitit][1].
I'm using it as the basis of my [electronic lab notebook][2],
and also trying to contribute back some plugins and other
enhancements. Changes from upstream so far:

* It's packaged using [Nix][3], which eliminates cabal hell. Use `nix-build` to
  build it or `nix-shell --pure` to enter a sandboxed shell with access to all
  the dependencies.

* Because Nix sandboxes all the GHC and cabal-related stuff for you,
  dynamically loading plugins is hard. I haven't gotten it to work.
  Instead when I write plugins I put them in [Network/Gitit/Plugins][4]
  and import them explicitly from [Plugins.hs][5].
  To improve runtime performance I also disable the `+plugins` compiler flag.

* I swapped out the [filestore][6] library for my own fork that follows symlinks
  transparently. That was necessary because my lab wiki stores a lot of large
  files (genomes, RNA-seq data, etc.) using [git-annex][7].

* I added a test wiki with pages to check if my plugins are working.
  To use it start gitit with `--config-file testwiki/testwiki.conf`.

Things I still intend to do but haven't gotten around to:

* SPARQL plugin on top of the CSV one so you can dynamically load queries
  as tables. Will be useful for keeping track of plates, plasmids, etc.

[1]: http://github.com/jgm/gitit
[2]: https://github.com/jefdaj/jeffwiki
[3]: http://nixos.org/nix/
[4]: 
[5]: 
[6]: 
[7]: 
