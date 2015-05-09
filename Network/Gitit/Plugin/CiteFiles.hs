module Network.Gitit.Plugin.CiteFiles
  where

{- This plugin supports keeping notes in the style used by Caleb McDaniel
 - (http://wcm1.web.rice.edu/plain-text-citations.html). That is, you have
 - one wiki page per citable source and it contains a `bib` codeblock with
 - its bibtex entry.
 -
 - Besides wiki pages, I also keep a few other related files on my wiki:
 - mostly PDFs (sometimes with handwritten notes overlaid), and
 - occasionally other odds and ends like spreadsheets, Word docs, or
 - videos. This plugin inserts a list of any related files at the top of
 - a page. It considers any file whose name starts with the name of the
 - current page "related". For example, if you have a wiki page called
 - kerby1972.page it would list kerby1972.pdf, kerby1972_ideas.txt, etc.
 -
 - See https://git-annex.branchable.com to efficiently store large files in
 - a git repository. Support for it has been added to the latest gitit.
 -
 - TODO rename to be clearer about what it does
 - TODO fix/report delete link spacing bug
 - TODO split off a Utilities module so any combination of plugins just
 -      needs that + the one you want?
 -}

import Network.Gitit.Interface

import Control.Exception (try, SomeException)
import Data.FileStore    (Resource(FSFile, FSDirectory), directory)
import Data.List         (intercalate, isPrefixOf)
import System.FilePath   (takeBaseName, addExtension, splitExtension)

askName :: PluginM String
askName = do
  req <- askRequest
  let base = takeBaseName $ rqUri req
  return base

listFiles :: FilePath -> PluginM [Resource]
listFiles dir = do
  fs  <- askFileStore
  res <- liftIO (try (directory fs dir) :: IO (Either SomeException [Resource]))
  case res of
    Left  _     -> return []
    Right files -> return files

resPath :: Resource -> FilePath
resPath (FSFile      f) = f
resPath (FSDirectory d) = d

reqDir :: Request -> FilePath
reqDir = intercalate "/" . init . rqPaths

-- TODO move to utilities?
render :: String -> [Resource] -> String
render prefix rs = show $ fileListToHtmlNoUplink "" prefix rs

renderFiles :: String -> [String] -> [Block]
renderFiles _ [] = []
renderFiles p fs = [header, html]
  where
    rs     = map FSFile fs
    html   = RawBlock (Format "html") (render p rs)
    header = Para [Str "Related files:"]

-- TODO don't assume .page?
isRelatedTo :: FilePath -> FilePath -> Bool
isRelatedTo p p2 = (pb `isPrefixOf` p2b) && not (p2 `elem` [pb, pp])
  where
    base = fst . splitExtension
    pb  = base p
    pp  = addExtension pb ".page"
    p2b = base p2

processDoc :: Pandoc -> PluginM Pandoc
processDoc (Pandoc m bs) = do
  n <- askName
  r <- askRequest
  let d = reqDir r
  fs <- listFiles d
  let ss = filter (isRelatedTo n) (map resPath fs)
      rs = renderFiles (d ++ "/") ss
  return $ Pandoc m (rs ++ bs)

plugin :: Plugin
plugin = PageTransform processDoc
