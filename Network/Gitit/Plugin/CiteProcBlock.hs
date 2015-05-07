module Network.Gitit.Plugin.CiteProcBlock
  where

import Network.Gitit.Interface
import Data.Maybe            (mapMaybe)
import Text.CSL.Input.Bibtex (readBibtexInputString)
import Text.CSL.Pandoc       (processCites)
import Text.CSL.Parser       (readCSLFile)
import Text.CSL.Reference    (Reference)
import Text.CSL.Style        (Style)

-- TODO write documentation with examples, like in the other plugins
-- TODO report minor href bug with DOI urls in pandoc-citeproc
-- TODO add custom PDF links from the bibtex branch

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

parseBibtex :: String -> IO [Reference]
parseBibtex = readBibtexInputString True

parseCitationStyle :: PluginM Style
parseCitationStyle = do
  cfg <- askConfig
  sty <- liftIO $ readCSLFile Nothing $ citationStyle cfg
  return sty

-- This tries reading the bibliography from blocks in the page,
-- then from a file specified in the page metadata,
-- and finally from a file specified in the wiki config file.
-- If they all fail it returns an empty reference list.
readBibliography :: Maybe String -> PluginM [Reference]
readBibliography blks = do
  conf <- askConfig
  meta <- askMeta
  let bibSources =
        [ fmap return   $ blks
        , fmap readFile $ lookup "bibliography" meta
        , fmap readFile $ defaultBibliography   conf
        , fmap return   $ Just ""
        ]
  txt <- liftIO $ head $ mapMaybe id bibSources
  bib <- liftIO $ parseBibtex txt
  return bib

processCiteBlocks :: Pandoc -> PluginM Pandoc
processCiteBlocks doc = do
  let (doc', bib) = separateBibliography doc
  bib' <- readBibliography bib
  sty  <- parseCitationStyle
  let doc'' = processCites sty bib' doc'
  return doc''

plugin :: Plugin
plugin = PageTransform processCiteBlocks
