module Network.Gitit.Plugin.CiteTitle
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
 -}

import Network.Gitit.Interface
import Network.Gitit.Plugin.CiteLinks (askName)
import Network.Gitit.Plugin.CiteProc  (getRefs)

import Data.Map            (insert)
import System.FilePath     (takeBaseName)
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
        Just [] -> return doc
        Just t  -> return $ setTitle doc t

decideTitle2 :: Pandoc -> PluginM Pandoc
decideTitle2 doc = do
  name <- askName
  refs <- getRefs doc
  let rmap = map keyAndTitle refs
  case lookup name rmap of
    Nothing -> return doc
    Just t  -> do
      let doc' = setTitle doc t
      return doc'

plugin :: Plugin
plugin = PageTransform decideTitle2
