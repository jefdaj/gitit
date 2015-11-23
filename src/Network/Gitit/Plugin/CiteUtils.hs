module Network.Gitit.Plugin.CiteUtils
  ( resPath
  , isPage
  , isThisPage
  , reqDir
  , listFiles
  , render
  , renderFiles
  )
  where
  -- ( blocksToString
  -- , setTitle
  -- , askName

import Network.Gitit.Interface
import Text.Pandoc.Definition

import Data.List (intercalate)
import Data.Map (insert)
import System.FilePath (takeBaseName)
import Data.FileStore (Resource(FSFile, FSDirectory), directory)
import Control.Exception (try, SomeException)
import Data.FileStore  (Resource(FSFile))
import Data.List       (isPrefixOf)
import System.FilePath (addExtension, splitExtension)
import Data.Maybe            (mapMaybe)
import Text.CSL.Input.Bibtex (readBibtexInputString)
import Text.CSL.Pandoc       (processCites)
import Text.CSL.Parser       (readCSLFile)
import Text.CSL.Reference    (Reference)
import Text.CSL.Style        (Style)
import Text.CSL.Reference (Reference, refId, title, unLiteral)
import Text.CSL.Style     (unFormatted)
import Data.FileStore  (Resource(FSFile))
import Data.List       (isPrefixOf)
import System.FilePath (addExtension, splitExtension)

-- blocksToString :: [Block] -> String
-- blocksToString bs = unlines $ map (\(CodeBlock _ t) -> t) bs
-- 
-- -- because Text.Pandoc.Builder.setTitle doesn't work on a [Inline]
-- setTitle :: Pandoc -> [Inline] -> Pandoc
-- setTitle (Pandoc m bs) t = Pandoc m' bs
--   where
--     old = unMeta m
--     new = insert "title" (MetaInlines t) old
--     m'  = Meta {unMeta = new}
-- 
-- -- TODO is this available from the Interface already?
-- askName :: PluginM String
-- askName = do
--   req <- askRequest
--   let base = takeBaseName $ rqUri req
--   return base

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

-- TODO is this the same as askFiles?
listFiles :: FilePath -> PluginM [Resource]
listFiles dir = do
  fs  <- askFileStore
  res <- liftIO (try (directory fs dir) :: IO (Either SomeException [Resource]))
  case res of
    Left  _     -> return []
    Right files -> return files

-- TODO if you export this, don't need fileListToHtmlNoUplink?
render :: String -> String -> [Resource] -> String
render prefix ext rs = show $ fileListToHtmlNoUplink "" prefix ext rs

renderFiles :: String -> String -> String -> [String] -> [Block]
renderFiles _ _ _ [] = []
renderFiles t p e fs = [header, html]
  where
    rs     = map FSFile fs
    html   = RawBlock (Format "html") (render p e rs)
    header = Para [Str t]
