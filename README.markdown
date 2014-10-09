Gitit
=====

This is my fork of [gitit][1].
I'm using it as the basis of my [electronic lab notebook][2],
and also trying to contribute back some plugins and other
enhancements. Things that work so far:

* You can point gitit at `testwiki/testwiki.conf` to start a
  simple test wiki.

* `gitit.sh` can automatically `build` or  `rebuild` gitit.
  It can also load the top-level functions in a  `repl`,
  or `serve` the test wiki.

* There are some extra bio-related wiki links in `Interwiki.hs`

* Added an explicit `cacheDir`.

* `Dot.hs` generates inline svg images now,
  which can include clickable links.

* The config file now contains a `plugin-dir` setting,
  and plugin locations are specified relative to that.

* The "external" plugin lets you make your own external page transform plugins.

* `Dot.hs` generates inline svg images now,
  which can include clickable links.

* Added an explicit `cacheDir`.


[1]: http://github.com/jgm/gitit
[2]: https://github.com/jefdaj/jeffwiki
