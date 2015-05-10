module Network.Gitit.Plugin.CiteProcTitle
  where

{- This plugin supports keeping notes in the style used by Caleb McDaniel
 - (http://wcm1.web.rice.edu/plain-text-citations.html). That is, you have
 - one wiki page per citable source and it contains a `bib` codeblock with
 - its bibtex entry. If the current page doesn't have a title but the
 - pagename matches one of its bibtex keys, it will set the title to that.
 -
 - It required adding some logic to Network.Gitit.ContentTransformer to
 - apply titles; the old code set them before creating the Pandoc and then
 - didn't check if a title was added afterward.
 -
 - TODO what will/should happen if you don't cite the paper in the page?
 -}

import Network.Gitit.Interface
import Network.Gitit.Plugin.CiteLinks (askName)
import Network.Gitit.Plugin.CiteProc  (getRefs)

import Data.Map            (insert)
import Text.CSL.Reference  (Reference, refId, title, unLiteral)
import Text.CSL.Style      (unFormatted)

-- I couldn't figure out getReference Locators, so I worked around them
keyAndTitle :: Reference -> (String, [Inline])
keyAndTitle r = (unId r, unTitle r)
  where
    unId    = unLiteral   . refId
    unTitle = unFormatted . title

-- because Text.Pandoc.Builder.setTitle doesn't work on a [Inline]
setTitle :: Pandoc -> [Inline] -> Pandoc
setTitle (Pandoc m bs) t = Pandoc m' bs
  where
    old = unMeta m
    new = insert "title" (MetaInlines t) old
    m'  = Meta {unMeta = new}

setTitleIfNeeded :: Pandoc -> PluginM Pandoc
setTitleIfNeeded doc = do
  name <- askName
  refs <- getRefs doc
  let rmap = map keyAndTitle refs
  case lookup name rmap of
    Nothing -> return doc
    Just t  -> return $ setTitle doc t

plugin :: Plugin
plugin = PageTransform setTitleIfNeeded
