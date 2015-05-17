module Network.Gitit.Plugin.CiteLinks
  where

{- This plugin supports keeping notes in the style used by Caleb McDaniel
 - (http://wcm1.web.rice.edu/plain-text-citations.html). That is, you have
 - one wiki page per citable source and it contains a `bib` codeblock with
 - its bibtex entry.
 -
 - If the current page includes a citation and there's a matching wiki page
 - in the same directory, it replaces the citation with a link to that
 - page. The idea is that if you already wrote notes you probably want to
 - go review them instead of jumping to the original source.
 -
 - Citations of the current source are ignored. Other bibtex is allowed
 - and will also be ignored (or hopefully passed on to my CiteProc plugin!)
 -}

import Network.Gitit.Interface

plugin :: Plugin
plugin = mkPageTransformM citeLinks

linkTo :: String -> Inline
linkTo name = Str $ "[" ++ name ++ "](" ++ name ++ ")"

citeLinks :: Inline -> PluginM Inline
citeLinks s@(Str ('@':name)) = do
  page <- isPage     name
  this <- isThisPage name
  if page && (not this)
    then return $ linkTo name
    else return s
citeLinks x = return x
