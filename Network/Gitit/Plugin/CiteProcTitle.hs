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
 - TODO document that this depends on having CiteProc imported + applied
 - TODO also add other fields below title? (author, publisher, year)
 -}

import Network.Gitit.Interface
import Network.Gitit.Plugin.CiteProc (getRefs)

import Text.CSL.Reference (Reference, refId, title, unLiteral)
import Text.CSL.Style     (unFormatted)

-- I couldn't figure out getReference Locators, so I worked around them
keyAndTitle :: Reference -> (String, [Inline])
keyAndTitle r = (unId r, unTitle r)
  where
    unId    = unLiteral   . refId
    unTitle = unFormatted . title

getThisRef :: Pandoc -> PluginM (Maybe [Inline])
getThisRef doc = do
  name <- askName
  refs <- getRefs doc
  let rmap = map keyAndTitle refs
  return $ lookup name rmap

setTitleIfNeeded :: Pandoc -> PluginM Pandoc
setTitleIfNeeded doc = do
  ref <- getThisRef doc
  case ref of
    Nothing -> return doc
    Just r  -> return $ setTitle doc r

plugin :: Plugin
plugin = PageTransform setTitleIfNeeded
