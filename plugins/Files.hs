module Files (plugin) where

import Control.Exception (try, SomeException)
import Data.Either
import Data.FileStore (Resource(FSFile, FSDirectory), directory)
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromMaybe)
import Network.Gitit.Interface


-- This plugin allows you to include a list of files
-- in a page. It's similar to "All pages" index,
-- but more flexible because you can filter the results
-- and show different subsets of your files,
-- maybe with separate explanations.
--
-- For example, this would list files in `dir1`
-- whose names include `.png` or `.jpg` but not `bad`:
--
-- ~~~ {.files dir="dir1"}
-- + .png
-- + .jpg
-- - bad
-- ~~~
--
-- If no `dir` attribute is given it defaults to the location
-- of the current page. If the block is empty it matches all files.

-- TODO add sorting so you can look at dates in reverse


--------------------
-- work with Gitit
--------------------

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, cs, as) txt) | "files" `elem` cs = do
  cfg <- askConfig
  req <- askRequest
  let reqdir   = reqDir req
      matchdir = fromMaybe reqdir $ lookup "dir" as
      prefix   = if null matchdir then "" else matchdir ++ "/"
  files <- listFiles matchdir
  let html = case conditions (lines txt) of
              Left  s  -> s
              Right cs -> let matches = restrict cs files
                          in show $ fileListToHtmlNoUplink "" prefix matches
  return $ RawBlock (Format "html") html
transformBlock x = return x


------------------------
-- work with FilePaths
------------------------

listFiles :: FilePath -> PluginM [Resource]
listFiles dir = do
  fs  <- askFileStore
  res <- liftIO (try (directory fs dir) :: IO (Either SomeException [Resource]))
  case res of
    Left  error -> return []
    Right files -> return files

resPath :: Resource -> FilePath
resPath (FSFile      f) = f
resPath (FSDirectory d) = d

reqDir :: Request -> FilePath
reqDir = intercalate "/" . init . rqPaths


-------------------------
-- work with Conditions
-------------------------

data Condition = Include String | Exclude String
  deriving Show

-- from http://stackoverflow.com/questions/19711730
trim :: String -> String
trim = unwords . words

condition :: String -> Either String Condition
condition ('+':cs) = Right $ Include (trim cs)
condition ('-':cs) = Right $ Exclude (trim cs)
condition s        = Left  $ "error: '" ++ s ++ "' is not a valid condition"

conditions :: [String] -> Either String [Condition]
conditions ss = if null failed then Right (rights parsed) else Left errmsg
  where
    parsed = map condition ss
    failed = lefts parsed
    errmsg = intercalate "<br />" $ map show failed

include :: String -> Resource -> Bool
include s r = s `isInfixOf` resPath r

exclude :: String -> Resource -> Bool
exclude s = not . include s

restrict :: [Condition] -> [Resource] -> [Resource]
restrict []             rs = rs
restrict (Include s:cs) rs = restrict cs $ filter (include s) rs
restrict (Exclude s:cs) rs = restrict cs $ filter (exclude s) rs
