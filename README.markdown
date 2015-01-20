Gitit
=====

This is my fork of [gitit][1].
I'm using it as the basis of my [electronic lab notebook][2],
and also trying to contribute back some plugins and other
enhancements.

Each feature I develop has its own `dev-<featurename>`
and `test-<featurename>` branches. The `dev-*` branches
start from `jgm`, which tracks upstream.
The `test-*` branches each get merged into my `master`,
which I use for the lab notebook.

The ones that work so far are:

* You can point gitit at `testwiki/testwiki.conf` to start a
  simple test wiki.


[1]: http://github.com/jgm/gitit
[2]: https://github.com/jefdaj/jeffwiki
