module Network.Gitit.Plugin.CiteLinks
  where

{- This plugin supports keeping notes in the style used by Caleb McDaniel
 - (http://wcm1.web.rice.edu/plain-text-citations.html). That is, you have
 - one wiki page per citable source and it contains a `bib` codeblock with
 - its bibtex entry. The plugin does two things:
 -
 - If the current page includes a citation and there's a matching wiki page
 - in the same directory, the citation will be replaced by a link to that
 - page. The idea is that if you already wrote notes you probably want to
 - go review them instead of jumping to the original source. Citations of
 - the current source are ignored.
 -
 - Other bibtex is allowed too and will be ignored (or hopefully passed on
 - to my CiteProc plugin!)
 -}

import Network.Gitit.Interface

replaceWord :: String -> PluginM String
replaceWord ('@':name) = do
  page <- isPage     name
  this <- isThisPage name
  if page && (not this)
    then return ("[" ++ name ++ "](" ++ name ++ ")")
    else return ('@':name)
replaceWord w = return w

-- TODO less ugly notation?
replaceInLine :: String -> PluginM String
replaceInLine line = do
  let ws = words line
  ws' <- mapM replaceWord ws
  let ws'' = unwords ws'
  return ws''

-- TODO less ugly notation?
replaceInPage :: String -> PluginM String
replaceInPage file = do
  let ls = lines file
  ls' <- mapM replaceInLine ls
  let ls'' = unlines ls'
  return ls''

plugin :: Plugin
plugin = PreParseTransform replaceInPage
