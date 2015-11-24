module Network.Gitit.Plugin.CiteProc
  where

-- TODO: move stuff back in here from CiteUtils unless it's really common

{- This plugin processes bibtex citations using pandoc-citeproc.
 - It will pick them up from one or more `bib` blocks in the page itself,
 - like so:
 -
 - ~~~{ .bib }
 - @Book{ kerby1972,
 -   author = {Robert L. Kerby},
 -   title = {Kirby Smith's Confederacy:
 -            The Trans-{M}ississippi South, 1863--1865},
 -   address = {New York},
 -   publisher = {Columbia University Press},
 -   year = 1972,
 - }
 - ~~~
 -
 - If there aren't any, it can also use a file specified in the page metadata:
 -
 - ---
 - title: My Page
 - bibliography: mybibforthispage.bib
 - ...
 -
 - If neither is specified it will fall back on `default-bibliography`
 - in your `pandoc-user-data` directory, both set in the main gitit
 - config file. You can also set a custom `citation-style` there,
 - which should point to a file in `pandoc-user-data/styles`.
 - There are thousands of styles available at http://www.zotero.org/styles
 - or http://github.com/citation-style-language/styles.
 -}

-- TODO make sure it doesn't fail on invalid bibtex
-- TODO report minor href bug with DOI urls in pandoc-citeproc
-- TODO add custom PDF links from the bibtex branch
-- TODO add a section for plugins to the config file,
--      then use it for the citeproc stuff to start out
-- TODO can docTitle from Pandoc used to get the title?

import Network.Gitit.Interface
-- import Network.Gitit.Plugin.CiteUtils (blocksToString, setTitle, askName)

import System.FilePath (takeBaseName)
import Data.Map (insert)
import Data.Maybe            (mapMaybe)
import Text.CSL.Input.Bibtex (readBibtexInputString)
import Text.CSL.Pandoc       (processCites)
import Text.CSL.Parser       (readCSLFile)
import Text.CSL.Reference    (Reference, refId, title, unLiteral)
import Text.CSL.Style        (Style, unFormatted)

blocksToString :: [Block] -> String
blocksToString bs = unlines $ map (\(CodeBlock _ t) -> t) bs

-- because Text.Pandoc.Builder.setTitle doesn't work on a [Inline]
setTitle :: Pandoc -> [Inline] -> Pandoc
setTitle (Pandoc m bs) t = Pandoc m' bs
  where
    old = unMeta m
    new = insert "title" (MetaInlines t) old
    m'  = Meta {unMeta = new}

-- TODO is this available from the Interface already?
askName :: PluginM String
askName = do
  req <- askRequest
  let base = takeBaseName $ rqUri req
  return base

isBibBlock :: Block -> Bool
isBibBlock (CodeBlock (_,cs,_) _) = "bib" `elem` cs
isBibBlock _ = False

extractRefs :: Pandoc -> (Pandoc, Maybe String)
extractRefs (Pandoc meta blks) = (Pandoc meta blks', bib')
  where
    blks' = filter (not . isBibBlock) blks
    bib   = filter isBibBlock blks
    bib'  = if null bib
              then Nothing
              else Just $ blocksToString bib

-- starting from Maybe String lets you treat all ref sources the same
parseRefs :: Maybe String -> PluginM [Reference]
parseRefs blks = do
  conf <- askConfig
  meta <- askMeta
  txt <- liftIO $ head $ mapMaybe id
    [ fmap return   $ blks
    , fmap readFile $ lookup "bibliography" meta
    , fmap readFile $ defaultBibliography   conf
    , fmap return   $ Just ""
    ]
  bib <- liftIO $ readBibtexInputString True txt
  return bib

getRefs :: Pandoc -> PluginM [Reference]
getRefs doc = do
  let (_, bib) = extractRefs doc
  bib' <- parseRefs bib
  return bib'

parseStyle :: PluginM Style
parseStyle = do
  cfg <- askConfig
  sty <- liftIO $ readCSLFile Nothing $ citationStyle cfg
  return sty

-- I couldn't figure out getReference Locators, so I worked around them
keyAndTitle :: Reference -> (String, [Inline])
keyAndTitle r = (unId r, unTitle r)
  where
    unId    = unLiteral   . refId
    unTitle = unFormatted . title

-- TODO have just getThisRef, and exract the title afterward?
--      (so you can get the other stuff too maybe)
getThisTitle :: Pandoc -> PluginM (Maybe [Inline])
getThisTitle doc = do
  name <- askName
  refs <- getRefs doc
  let rmap = map keyAndTitle refs
  return $ lookup name rmap

setTitleIfNeeded :: Pandoc -> PluginM Pandoc
setTitleIfNeeded doc = do
  ref <- getThisTitle doc
  case ref of
    Nothing -> return doc
    Just r  -> return $ setTitle doc r

-- TODO is it even using the CSL file, or does that require processCites'?
--      also, does processCites' take care of loading the bibliography?
-- TODO make sure not to set title if there is one in the metadata already
processDoc :: Pandoc -> PluginM Pandoc
processDoc doc = do
  ttl <- getThisTitle doc
  let (doc', bib) = extractRefs doc
  bib' <- parseRefs bib
  sty  <- parseStyle
  let doc'' = processCites sty bib' doc'
  case ttl of
    Nothing -> return doc''
    Just t  -> return $ setTitle doc'' t

plugin :: Plugin
plugin = PageTransform processDoc
