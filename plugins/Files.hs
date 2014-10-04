module Files (plugin) where

-- This plugin allows you to include a list of files
-- in a page. It's similar to "All pages" index,
-- but more flexible because you can filter the results
-- and show different subsets of your files,
-- maybe with separate explanations.
--
-- For example, this would only list files
-- in `dir1` that have `png` or `jpg` in their name:
--
-- ~~~ {.files dir="dir1"}
-- png
-- jpg
-- ~~~
--
-- If no `dir` attribute is given it defaults to the location
-- of the current page. If the block is empty it matches all files.

import Control.Exception (try, SomeException)
import Data.FileStore (Resource(FSFile, FSDirectory), directory)
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromMaybe)
import Network.Gitit.Interface

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, cs, as) txt) | elem "files" cs = do
  cfg <- askConfig
  req <- askRequest
  let reqdir   = reqDir req
      matchdir = fromMaybe reqdir $ lookup "dir" as
      prefix   = if null matchdir then "" else matchdir ++ "/"
  files <- listFiles matchdir
  let matches = filterPaths (lines txt) files
      html    = fileListToHtmlNoUplink "" prefix matches
  return $ RawBlock (Format "html") (show html)
transformBlock x = return x

listFiles :: FilePath -> PluginM [Resource]
listFiles dir = do
  fs  <- askFileStore
  res <- liftIO $ (try (directory fs dir) :: IO (Either SomeException [Resource]))
  case res of
    Left  error -> return []
    Right files -> return files

resPath :: Resource -> FilePath
resPath (FSFile      f) = f
resPath (FSDirectory d) = d

reqDir :: Request -> FilePath
reqDir req = (intercalate "/" . init . rqPaths) req

anyInPath :: [String] -> Resource -> Bool
anyInPath searches path = any (\s -> isInfixOf s (resPath path)) searches

filterPaths :: [String] -> [Resource] -> [Resource]
filterPaths [] fs = fs
filterPaths ss fs = filter (anyInPath ss) fs
