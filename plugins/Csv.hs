module Csv (plugin) where

-- parts of this are based on:
-- bonsaicode.wordpress.com/2013/01/15/programming-praxis-translate-csv-to-html

-- TODO have and optional `class="header"` rather than pattern matching
-- TODO make 'file' paths relative to repository-path when absolute
--      and relative to repository-path/cleaned-up-uri if relpaths
-- TODO handle ragged spreadsheets?

import Data.List
import Data.List.Split
import Network.Gitit.Interface
import System.Directory
import System.FilePath.Posix

-----------------
-- CSV to HTML --
-----------------

row :: String -> String -> Maybe String -> String -> String
row outer inner cls s = wrap outer cls
                      $ concatMap (wrap inner Nothing)
                      $ splitOn "," s

tr :: String -> Maybe String -> String -> String
tr = row "tr"

thead :: String -> String
thead s = wrap "thead" Nothing
        $ tr "th" (Just "header") s

tbody :: [String] -> String
tbody ss = wrap "tbody" Nothing
         $ concatMap (tr "td" Nothing) ss

table :: String -> String
table s = let t = wrap "table" Nothing
          in case (lines s) of
            []     -> tbody [""]
            [l]    -> t $ tbody [tr "td" Nothing l]
            (l:ls) -> t $ (thead l) ++ (tbody ls)

-- takes a tag, an optional class, and text to wrap
-- returns the text wrapped in the tag
wrap :: String -> Maybe String -> String -> String
wrap tag cls txt = "<" ++ tag ++ cls' ++ ">" ++ txt ++ "</" ++ tag ++ ">"
  where
    cls' = case cls of
             Nothing -> ""
             Just c  -> " class=\"" ++ c ++ "\" "


-------------------------
-- find and load files --
-------------------------

uri2path :: String -> FilePath
uri2path uri
  = intercalate [pathSeparator]
  $ filter (/= "")                        -- remove blanks
  $ filter (\s -> not $ isPrefixOf "_" s) -- remove _edit etc.
  $ splitOn [pathSeparator] uri

-- returns the path to the .page file associated with a request
askFile :: PluginM FilePath
askFile = do
  cfg <- askConfig
  req <- askRequest
  let p1 = repositoryPath cfg
      p2 = uri2path $ rqUri req
      p  = joinPath [p1, p2]
  return p

-- takes an absolute or relative gitit link
-- and makes it into a file path relative to the running program
link2path :: String -> PluginM FilePath
link2path lnk = do
  cfg <- askConfig
  pfp <- askFile
  -- if it starts with "/", the link is relative to the repository
  -- otherwise, it's relative to the requested page
  return $ joinPath $ if "/" `isPrefixOf` lnk
    then [repositoryPath cfg, dropWhile (== '/') lnk]
    else [takeDirectory pfp, lnk]

-- gets block attributes and body text, and
-- reads a file to replace the text if needed
body :: [(String, FilePath)] -> String -> PluginM String
body as txt = case lookup "file" as of
  Nothing -> return txt
  Just f  -> do
    p <- link2path f
    e <- liftIO $ doesFileExist p
    case e of
      False -> return $ "file not found: " ++ p
      True  -> liftIO $ readFile p


------------------
-- gitit plugin --
------------------

plugin :: Plugin
plugin = mkPageTransformM blockTransform

blockTransform :: Block -> PluginM Block
blockTransform (CodeBlock (_, cs, as) txt) | elem "csv" cs = do
  bod <- body as txt
  return $ RawBlock (Format "html") (table bod)
blockTransform x = return x
