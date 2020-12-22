{-# LANGUAGE OverloadedStrings #-}

module Network.Gitit.Plugins.CiteLinks
  where

-- TODO OK, something about the CiteProc plugin is messing this up :( find
-- and fix!

{- This plugin supports keeping notes in the style used by Caleb McDaniel
 - (http://wcm1.web.rice.edu/plain-text-citations.html). That is, you have
 - one wiki page per citable source and it contains a `bib` codeblock with
 - its bibtex entry. Separate bibtex files are also supported.
 -
 - If the current page includes a citation and there's a matching wiki page
 - in the same directory, this replaces the citation with a link to that
 - page. The idea is that if you already wrote notes you probably want to
 - go review them instead of jumping straight to the original source.
 -
 - Citations of the current page are ignored. Other bibtex is allowed and
 - will also be ignored (then hopefully passed on to my CiteProc plugin!)
 -
 - TODO need to identify links however CiteProc does it?
 -      it's not just a simple @ followed by letters
 -      if not, document that it probably only works with Markdown
 -}

import Network.Gitit.Interface
import Network.Gitit.Plugins.CiteUtils
import Data.Text (head, tail, null, unpack)
import Prelude hiding (head, tail, null)

plugin :: Plugin
plugin = mkPageTransformM citeLinks

-- TODO rewrite to pick up already-processed citations from citeproc instead
-- TODO hmm what if it's just not counting the right things as pages,
--      like because of the new default extension argument?
citeLinks :: Inline -> PluginM Inline
citeLinks (Str s) | not (null s) && head s == '@' = do
  let name = tail s
      link = Link nullAttr [] (name, name)
  page <- isPage     $ unpack name
  this <- isThisPage $ unpack name
  liftIO $ putStrLn $ "citelinks page: " ++ show page
  liftIO $ putStrLn $ "citelinks this: " ++ show this
  liftIO $ putStrLn $ "citelinks link: " ++ show link
  if page && (not this)
    then return link
    else return (Str s)
citeLinks x = do
  liftIO $ putStrLn $ "citelinks x: " ++ show x
  return x
