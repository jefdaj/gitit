module Network.Gitit.Plugin.CiteProcBlock
  where

import Network.Gitit.Interface
import Data.List             (intercalate)
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
 - * write documentation with examples, like in the other plugins
 - * add custom PDF links from the bibtex branch
 - * report minor href bug with DOI urls in pandoc-citeproc:
 -   http://doi.org/<a href="http://dx.doi.org/10.3390/life5010403">10.3390/life5010403</a>
 -}

type Bibtex = String

isBibBlock :: Block -> Bool
isBibBlock (CodeBlock (_,cs,_) _) = "bib" `elem` cs
isBibBlock _ = False

toBibtex :: [Block] -> Bibtex
toBibtex bs = intercalate "\n" $ map (\(CodeBlock _ t) -> t) bs

extractBibtex :: Pandoc -> (Pandoc, Bibtex)
extractBibtex (Pandoc meta blks) = (Pandoc meta blks', toBibtex bibs)
  where
    blks' = filter (not . isBibBlock) blks
    bibs  = filter isBibBlock blks

readBibtex :: Bibtex -> IO [Reference]
readBibtex = readBibtexInputString True

readStyleFile :: PluginM Style
readStyleFile = do
  cfg <- askConfig
  sty <- liftIO $ readCSLFile Nothing $ defaultCitationStyle cfg
  return sty

processCiteBlocks :: Pandoc -> PluginM Pandoc
processCiteBlocks doc = do
  let (doc', bib) = extractBibtex doc
  sty  <- readStyleFile
  refs <- liftIO $ readBibtex bib
  let doc'' = processCites sty refs doc'
  return doc''

plugin :: Plugin
plugin = PageTransform processCiteBlocks
