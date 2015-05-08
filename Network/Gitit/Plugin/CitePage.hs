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

 - TODO implement ^that if not too hard
 -  get just the bibliography (borrow code from citeprocblock)
 -  parse into (key, title) pairs
 -  lookup the current name to get the title, if any
 -  only do that if there isn't a title already
 -
 - * If the current page includes a citation and there's a matching
 -   wiki page in the same directory, the citation will be replaced by a
 -   link to that page. The idea is that if you already wrote notes you
 -   probably want to go review them instead of jumping to the original
 -   source. Citations of the current source are ignored.
 -
 - Other bibtex is allowed too and will be ignored (and hopefully passed on
 - to my CiteProcBlock plugin!)
 -}

import Network.Gitit.Interface
import Control.Exception (try, SomeException)
import Data.FileStore    (Resource(FSFile, FSDirectory), directory)
import Data.List         (intercalate)
import System.FilePath   (takeBaseName, (</>))

-- TODO is this available from the Interface already?
askName :: PluginM FilePath
askName = do
  req <- askRequest
  let base = takeBaseName $ rqUri req
  return base

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

processLine :: String -> PluginM String
processLine line = do
  let ws = words line
  ws' <- mapM processWord ws
  let ws'' = unwords ws'
  return ws''

processPage :: String -> PluginM String
processPage file = do
  let ls = lines file
  ls' <- mapM processLine ls
  let ls'' = unlines ls'
  return ls''

plugin :: Plugin
plugin = PreParseTransform processPage
