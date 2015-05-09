module Network.Gitit.Plugin.CitePage
  where

{- This plugin supports keeping notes in the style used by Caleb McDaniel
 - (http://wcm1.web.rice.edu/plain-text-citations.html). That is, you have
 - one wiki page per citable source and it contains a `bib` codeblock with
 - its bibtex entry. The plugin does two things:
 -
 - * If the current page doesn't have a title, but the URL matches
 -   one of its bibtex keys, the title for that bibtex entry will be used.
 -   If there's no title the key itself is used.

 - * If the current page includes a citation and there's a matching
 -   wiki page in the same directory, the citation will be replaced by a
 -   link to that page. The idea is that if you already wrote notes you
 -   probably want to go review them instead of jumping to the original
 -   source. Citations of the current source are ignored.
 -
 - TODO also add the pdf linking? Would be easy once you have the title...
 -      if that's in too, rename to "fancy" something
 -
 - Other bibtex is allowed too and will be ignored (or hopefully passed on
 - to my CiteProc plugin!)
 -}

import Network.Gitit.Interface
import Network.Gitit.Plugin.CiteProc (separateBibliography, readBibliography)

import Control.Exception  (try, SomeException)
import Data.FileStore     (Resource(FSFile, FSDirectory), directory)
import Data.List          (intercalate)
import Data.Map           (union, fromList)
import System.FilePath    (takeBaseName)
import Text.CSL.Reference (Reference, refId, titleShort, unLiteral)
import Text.CSL.Style     (unFormatted)

----------------------
-- shared utilities --
----------------------

-- TODO is this available from the Interface already?
askName :: PluginM FilePath
askName = do
  req <- askRequest
  let base = takeBaseName $ rqUri req
  return base

--------------------
-- set page title --
--------------------

getBibliography :: Pandoc -> PluginM [Reference]
getBibliography doc = do
  let (_, bib) = separateBibliography doc
  bib' <- readBibliography bib
  return bib'

setPageTitle :: Pandoc -> PluginM Pandoc
setPageTitle doc@(Pandoc m bs) = do
  bib  <- getBibliography doc
  meta <- askMeta
  case lookup "title" meta of
    Just _ -> return doc
    Nothing -> do
      name <- askName
      let titles = map keyAndTitle bib
          title  = lookup name titles
      case title of
        Nothing -> return doc
        Just t  -> do
          let old  = unMeta m
              new  = fromList [("title", MetaInlines t)]
              both = Meta {unMeta = union new old}
              doc' = Pandoc both bs
          return doc'

-- I couldn't figure out getReference Locators, so I worked around them
keyAndTitle :: Reference -> (String, [Inline])
keyAndTitle r = (unId r, unTitle r)
  where
    unId    = unLiteral   . refId
    unTitle = unFormatted . titleShort

--------------------
-- link citations --
--------------------

resPath :: Resource -> FilePath
resPath (FSFile      f) = f
resPath (FSDirectory d) = d

reqDir :: Request -> FilePath
reqDir = intercalate "/" . init . rqPaths

askFiles :: PluginM [FilePath]
askFiles = do
  fs  <- askFileStore
  req <- askRequest
  let dir = reqDir req
  res <- liftIO (try (directory fs dir) :: IO (Either SomeException [Resource]))
  case res of
    Left  _     -> return []
    Right files -> return $ map (takeBaseName . resPath) files

isPage :: FilePath -> PluginM Bool
isPage name = do
  files <- askFiles
  return $ name `elem` (map takeBaseName files)

isThisPage :: FilePath -> PluginM Bool
isThisPage name1 = do
  name2 <- askName
  return $ name1 == name2

processWord :: String -> PluginM String
processWord ('@':name) = do
  page <- isPage     name
  this <- isThisPage name
  if page && (not this)
    then return ("[" ++ name ++ "](" ++ name ++ ")")
    else return ('@':name)
processWord w = return w

-- TODO more elegant notation?
processLine :: String -> PluginM String
processLine line = do
  let ws = words line
  ws' <- mapM processWord ws
  let ws'' = unwords ws'
  return ws''

-- TODO more elegant notation?
processPage :: String -> PluginM String
processPage file = do
  let ls = lines file
  ls' <- mapM processLine ls
  let ls'' = unlines ls'
  return ls''

----------
-- main --
----------

plugin :: Plugin
plugin = PreParseTransform processPage
