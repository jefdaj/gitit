module Network.Gitit.Plugin.CiteProcBlock
  where

{- This plugin processes bibtex citations using pandoc-citeproc.
 - It will pick them up from one or more `bib` blocks in the page itself,
 - like so:
 -
 - ~~~{ .bib }
 - @Book{ kerby1972,
 -   author = {Robert L. Kerby},
 -   title = {Kirby Smith's Confederacy: The Trans-{M}ississippi South, 1863--1865},
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

-- TODO report minor href bug with DOI urls in pandoc-citeproc
-- TODO add custom PDF links from the bibtex branch

import Network.Gitit.Interface
import Data.Maybe            (mapMaybe)
import Text.CSL.Input.Bibtex (readBibtexInputString)
import Text.CSL.Pandoc       (processCites)
import Text.CSL.Parser       (readCSLFile)
import Text.CSL.Reference    (Reference)
import Text.CSL.Style        (Style)

isBibBlock :: Block -> Bool
isBibBlock (CodeBlock (_,cs,_) _) = "bib" `elem` cs
isBibBlock _ = False

blocksToString :: [Block] -> String
blocksToString bs = unlines $ map (\(CodeBlock _ t) -> t) bs

separateBibliography :: Pandoc -> (Pandoc, Maybe String)
separateBibliography (Pandoc meta blks) = (Pandoc meta blks', bib')
  where
    blks' = filter (not . isBibBlock) blks
    bib   = filter isBibBlock blks
    bib'  = if null bib
              then Nothing
              else Just $ blocksToString bib

readCitationStyle :: PluginM Style
readCitationStyle = do
  cfg <- askConfig
  sty <- liftIO $ readCSLFile Nothing $ citationStyle cfg
  return sty

readBibliography :: Maybe String -> PluginM [Reference]
readBibliography blks = do
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

processCiteBlocks :: Pandoc -> PluginM Pandoc
processCiteBlocks doc = do
  let (doc', bib) = separateBibliography doc
  bib' <- readBibliography bib
  sty  <- readCitationStyle
  let doc'' = processCites sty bib' doc'
  return doc''

plugin :: Plugin
plugin = PageTransform processCiteBlocks
