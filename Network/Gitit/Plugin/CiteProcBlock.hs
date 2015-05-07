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
 -
 - Citeproc makes URLs clickable, and this plugin also extends that to
 - PDFs: each reference is followed by a link to a PDF of the same name if
 - one exists. For example, if you add a file `kerby1972.pdf` in the same
 - directory as the page used in the example above, then
 - `<a href="kerby1972.pdf">kerby1972.pdf</>` will be added next to the end
 - of the bibliography entry. See http://git-annex.branchable.com/ for
 - efficient version control of large files in git.
 -
 - TODO also do other filetypes? what about pages?
 - TODO report minor href bug with DOI urls in pandoc-citeproc
 -}

import Network.Gitit.Interface
import Control.Exception     (try, SomeException)
import Data.FileStore        (Resource(..), FileStore(..), directory)
import Data.List             (intercalate)
import Data.Maybe            (mapMaybe)
import Text.CSL.Input.Bibtex (readBibtexInputString)
import Text.CSL.Pandoc       (processCites)
import Text.CSL.Parser       (readCSLFile)
import Text.CSL.Reference    (Reference, refId, unLiteral)
import Text.CSL.Style        (Style)


-- make bibliography --

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


-- add pdf links --

reqDir :: Request -> FilePath
reqDir = intercalate "/" . init . rqPaths

pdfLink :: String -> PluginM Inline
pdfLink key = do
  req  <- askRequest
  stor <- askFileStore
  let dir  = reqDir req
      name = key ++ ".pdf"
  sdir <- liftIO (try (directory stor dir) :: IO (Either SomeException [Resource]))
  return $ case sdir of
    Left  _ -> Str ""
    Right d -> if (FSFile name) `elem` d
                 then Link [Str "name"] (name, [])
                 else Str ""

{- The actual Reference type looks really complicated,
 - so I insert links into the final Pandoc instead.
 - But they need to be made from the References because the
 - final output doesn't have citation keys anymore.
 - So this function makes a list of links...
 -}
makePdfLinks :: [Reference] -> PluginM [Inline]
makePdfLinks refs = do
  let keys = map (unLiteral . refId) refs
  pdfs <- mapM pdfLink keys
  return pdfs

-- and this one adds them to the document.
addPdfLinks :: Pandoc -> [Inline] -> PluginM Pandoc
addPdfLinks d is = return d


-- process document --

processDoc :: Pandoc -> PluginM Pandoc
processDoc doc = do
  let (doc', bib) = separateBibliography doc
  bib' <- readBibliography bib
  pdfs <- makePdfLinks bib'
  sty  <- readCitationStyle
  let doc'' = processCites sty bib' doc'
  doc''' <- addPdfLinks doc'' pdfs
  return doc'''

plugin :: Plugin
plugin = PageTransform processDoc
