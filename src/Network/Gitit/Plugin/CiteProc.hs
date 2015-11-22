module Network.Gitit.Plugin.CiteProc
  where

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

import Network.Gitit.Interface

import Data.Maybe            (mapMaybe)
import Text.CSL.Input.Bibtex (readBibtexInputString)
import Text.CSL.Pandoc       (processCites)
import Text.CSL.Parser       (readCSLFile)
import Text.CSL.Reference    (Reference)
import Text.CSL.Style        (Style)

defaultBibliography :: FilePath
defaultBibliography = "/home/jefdaj/code/nixcfg/mypkgs/gitit/default.bib"

defaultStyle :: FilePath
defaultStyle = "/home/jefdaj/code/nixcfg/mypkgs/gitit/apa"

blocksToString :: [Block] -> String
blocksToString bs = unlines $ map (\(CodeBlock _ t) -> t) bs

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
    , fmap readFile $ Just defaultBibliography
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
  sty <- liftIO $ readCSLFile Nothing $ defaultStyle
  return sty

processDoc :: Pandoc -> PluginM Pandoc
processDoc doc = do
  let (doc', bib) = extractRefs doc
  bib' <- parseRefs bib
  sty  <- parseStyle
  let doc'' = processCites sty bib' doc'
  return doc''

plugin :: Plugin
plugin = PageTransform processDoc
