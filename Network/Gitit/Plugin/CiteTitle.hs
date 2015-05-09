module Network.Gitit.Plugin.CiteTitle
  where

{- This plugin supports keeping notes in the style used by Caleb McDaniel
 - (http://wcm1.web.rice.edu/plain-text-citations.html). That is, you have
 - one wiki page per citable source and it contains a `bib` codeblock with
 - its bibtex entry.
 -
 - If the current page doesn't have a title, but the URL matches one of its
 - bibtex keys, it will set the title to that. If there's no citation title
 - either the key itself is used.
 -
 - TODO also add the pdf linking? Would be easy once you have the title...
 -      if that's in too, rename to "fancy" something
 -}

import Network.Gitit.Interface
import Network.Gitit.Plugin.CiteLinks (askName)
import Network.Gitit.Plugin.CiteProc  (getRefs)

import Data.Map            (union, fromList)
import System.FilePath     (takeBaseName)
import Text.CSL.Reference  (Reference, refId, titleShort, unLiteral)
import Text.CSL.Style      (unFormatted)

-- I couldn't figure out getReference Locators, so I worked around them
keyAndTitle :: Reference -> (String, [Inline])
keyAndTitle r = (unId r, unTitle r)
  where
    unId    = unLiteral   . refId
    unTitle = unFormatted . titleShort

-- because Text.Pandoc.Builder.setTitle doesn't work on a [Inline]
setTitle :: Pandoc -> [Inline] -> Pandoc
setTitle (Pandoc m bs) title = Pandoc m' bs
  where
    old = unMeta m
    new = fromList [("title", MetaInlines title)]
    m'  = Meta {unMeta = union new old}

isBadTitle :: PluginM Bool
isBadTitle = do
  meta <- askMeta
  name <- askName
  case lookup "title" meta of
    Nothing -> return True
    Just t  -> return $ name == takeBaseName t

decideTitle :: Pandoc -> PluginM Pandoc
decideTitle doc = do
  bad <- isBadTitle
  if not bad
    then return doc
    else do
      refs <- getRefs doc
      name <- askName
      case lookup name $ map keyAndTitle refs of
        Nothing -> return doc
        Just t  -> return $ setTitle doc t

plugin :: Plugin
plugin = PageTransform decideTitle
