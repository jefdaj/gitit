Gitit
=====

My fork of [gitit](https://github.com/jgm/gitit).
This is the master branch, which is messy and consists of all my changes
jumbled together. They can be found separately in the `feat/*` and `pullreq/*`
branches. Here's a summary:

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
