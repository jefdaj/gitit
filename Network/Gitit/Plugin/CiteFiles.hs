module Network.Gitit.Plugin.CiteFiles
  where

{- This plugin supports keeping notes in the style used by Caleb McDaniel
 - (http://wcm1.web.rice.edu/plain-text-citations.html). That is, you have
 - one wiki page per citable source and it contains a `bib` codeblock with
 - its bibtex entry.
 -
 - Besides wiki pages, I also keep a few other related files on my wiki:
 - mostly PDFs (sometimes with handwritten notes overlaid), and
 - occasionally other odds and ends like spreadsheets, Word docs, or
 - videos. This plugin inserts a list of any related files at the top of
 - a page. It considers any file whose name starts with the name of the
 - current page "related". For example, if you have a wiki page called
 - kerby1972.page it would list kerby1972.pdf, kerby1972_ideas.txt, etc.
 -
 - See https://git-annex.branchable.com to efficiently store large files in
 - a git repository. Support for it was recently added to gitit (2015-05).
 -
 - TODO implement file links :)
 - TODO split off a Utilities module so any combination of plugins just
 -      needs that + the one you want?
 -}

import Network.Gitit.Interface

plugin :: Plugin
plugin = undefined
