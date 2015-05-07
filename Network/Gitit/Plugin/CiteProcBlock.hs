module Network.Gitit.Plugin.CiteProcBlock
  where

import Network.Gitit.Interface
import Data.Maybe            (mapMaybe)
import Text.CSL.Input.Bibtex (readBibtexInputString)
import Text.CSL.Pandoc       (processCites)
import Text.CSL.Parser       (readCSLFile)
import Text.CSL.Reference    (Reference)
import Text.CSL.Style        (Style)

{- TODO:
 - x split file into bib blocks and others
 - x see if a temp file is required
 - x   if not, just pass to citeproc without the PluginM?
 - x pass the non-bib blocks along and parse normally
 - x write main plugin function
 - * add zotero styles (https://www.zotero.org/styles/apa) to config file
 - * find bibliography in config file or page metadata too
 - * write documentation with examples, like in the other plugins
 - * add custom PDF links from the bibtex branch
 - * report minor href bug with DOI urls in pandoc-citeproc:
 -   http://doi.org/<a href="http://dx.doi.org/10.3390/life5010403">10.3390/life5010403</a>
 -}

isBibBlock :: Block -> Bool
isBibBlock (CodeBlock (_,cs,_) _) = "bib" `elem` cs
isBibBlock _ = False

blocksToString :: [Block] -> String
blocksToString bs = unlines $ map (\(CodeBlock _ t) -> t) bs

separateBibliography :: Pandoc -> (Pandoc, Maybe String)
separateBibliography (Pandoc meta blks) = (Pandoc meta blks', bib')
  where
    blks' = filter (not . isBibBlock) blks
    bib   = filter isBibBlock blks -- :: [Block]
    bib'  = if null bib
              then Nothing
              else Just $ blocksToString bib

readBibtex :: String -> IO [Reference]
readBibtex = readBibtexInputString True

readStyleFile :: PluginM Style
readStyleFile = do
  cfg <- askConfig
  sty <- liftIO $ readCSLFile Nothing $ defaultCitationStyle cfg
  return sty

-- This tries reading the bibliography from blocks in the page,
-- then from a file specified in the page metadata,
-- and finally from a file specified in the wiki config file.
-- If they all fail it returns an empty reference list.
-- TODO need to read from the filestore, not regular filesystem!
makeBibliography :: Maybe String -> PluginM [Reference]
makeBibliography blks = do
  conf <- askConfig
  meta <- askMeta
  let bibSources =
        [ fmap return   $ blks
        , fmap readFile $ lookup "bibliography" meta
        , fmap readFile $ defaultBibliography   conf
        , fmap return   $ Just ""
        ]
  txt <- liftIO $ head $ mapMaybe id bibSources
  bib <- liftIO $ readBibtex txt
  return bib

processCiteBlocks :: Pandoc -> PluginM Pandoc
processCiteBlocks doc = do
  let (doc', bib) = separateBibliography doc
  bib' <- makeBibliography bib
  sty  <- readStyleFile
  let doc'' = processCites sty bib' doc'
  return doc''

plugin :: Plugin
plugin = PageTransform processCiteBlocks
