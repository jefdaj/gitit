{-# LANGUAGE OverloadedStrings #-}

module Network.Gitit.Plugins.CiteLinks
  where

-- TODO is it possible to make the link text (inline Str) be the title?
-- TODO make url relative to a default dir?

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
import Data.Text (head, tail, null, pack, unpack)
import Prelude hiding (head, tail, null)

plugin :: Plugin
plugin = mkPageTransformM citeLinks

citeLinks :: Inline -> PluginM Inline
citeLinks x@(Cite (c:_) _) = do
  let cid  = citationId c
      link = Link nullAttr [Str cid] (cid, cid)
  page <- isPage     $ unpack cid
  this <- isThisPage $ unpack cid
  -- liftIO $ putStrLn $ "citelinks cite: " ++ show c
  -- liftIO $ putStrLn $ "citelinks link: " ++ show link
  if page && (not this)
    then return link
    else return x
citeLinks x = return x
