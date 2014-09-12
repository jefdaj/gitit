module Network.Gitit.Plugin.External
  ( plugin
  , mkPlugin
  , Plugin
  , allArgs
  , askFile
  , link2path
  ) where

import Control.Monad.IO.Class
import Data.List
import Data.List.Split
import Data.Maybe
import Network.Gitit.Interface
import System.Exit
import System.FilePath
import System.Process
-- import Paths_gitit (getDataFileName)

{- Passes text to an external script as stdin
 - and returns with the script's stdout
 - (or debugging info if there's an error)
 -}
eval :: FilePath -> [String] -> String -> IO String
eval "" _ _ = return "ERROR: missing 'bin' attribute"
eval bin args txt = do
  (code, out, err) <- readProcessWithExitCode bin args txt
  if code == ExitSuccess
    then return out
    else return $ unlines
      [ "ERROR:  " ++ (show bin) ++ " returned " ++ (show code)
      , "args:   " ++ (show args)
      , "stdin:  " ++ (show txt)
      , "stdout: " ++ (show out)
      , "stderr: " ++ (show err)
      ]


{- Takes a format string and some text, and
 - wraps the text in the corresponding Pandoc Block
 -}
wrap :: String -> String -> Block
wrap "html" txt = RawBlock (Format "html") txt
wrap "csv"  txt = CodeBlock ("", ["csv"], []) txt
wrap "list" txt = BulletList [map (\s -> Plain [Str s]) $ lines txt]
wrap "para" txt = Para  [Str txt]
wrap _      txt = Plain [Str txt]


mkArgs :: [String] -> [(String, String)] -> [String]
mkArgs ok usr = concat $ map flagify $ screen usr
  where
    screen = filter (\(k,_) -> elem k ok)
    flagify (k, v) = ["--" ++ k, v]


-- asks for the plugin data available from gitit and
-- formats it as command line args for external scripts
-- also takes a predicate for filtering which args to use
argList :: [String] -> [(String, String)] -> PluginM [String]
argList ask usr = do
  c <- askConfig
  m <- askMeta
  r <- askRequest
  return $ mkArgs (ask ++ map fst usr) $ concat [usr, m, cfgFlags c, reqFlags r]
  where
    reqFlags r = [("uri", rqUri r)]
    cfgFlags c =
      [ ("repository-path", repositoryPath c)
      , ("templates-dir"  , templatesDir   c)
      , ("static-dir"     , staticDir      c)
      , ("plugin-dir"     , pluginDir      c)
      , ("cache-dir"      , cacheDir       c)
      ]

allArgs :: [String]
allArgs =
  [ "repository-path"
  , "templates-dir"
  , "static-dir"
  , "plugin-dir"
  , "cache-dir"
  , "uri"
  ]

-- TODO add ask to the documentation
-- TODO remove other "external" plugin?

{- This renders generic "external" codeblocks using whatever
 - command you want. The 'bin' attribute is required,
 - but 'fmt' defaults to "plain" (plain text) and 'ask' to [].
 -
 - Other fmt options are:
 -   "para" for plain text as a paragraph
 -   "csv"  for a table of comma-separated values
 -   "list" for a bullet list
 -   "html" for raw html
 -
 - 'ask' should be a whitespace-separated list of args
 - you'd like this plugin to pass on to your script.
 - The options are:
 -   repository-path
 -   templates-dir
 -   static-dir
 -   plugin-dir
 -   cache-dir
 -   uri
 -
 - Here's how you might use it:
 -
 - > ~~~ { .external bin="/path/to/my/binary" fmt="html" ask="uri cache-dir" }
 - > gitit will run '/path/to/my/binary --uri <page uri> --cache-dir <cache dir>'
 - > this text will be sent as stdin,
 - > and then stdout will be wrapped in RawBlock "html"
 - > ~~~
 -}
plugin :: Plugin
plugin = mkPageTransformM tfm
  where
    tfm :: Block -> PluginM Block
    tfm (CodeBlock (_, cs, as) txt) | elem "external" cs = do
      let bin = fromMaybe "" $ lookup "bin" as
          fmt = fromMaybe "" $ lookup "fmt" as
          nfo = fromMaybe "" $ lookup "nfo" as
      args <- argList (words nfo) as
      bin' <- findBinary bin
      out  <- liftIO $ eval bin' args txt
      return $ wrap fmt out
    tfm x = return x

-- TODO get this to work with plugins in the ghc data dir too?
-- TODO and in whatever logic parses the config file
findBinary :: FilePath -> PluginM FilePath
findBinary b = case isPrefixOf "/" b of
                True  -> return b
                False -> do
                  cfg <- askConfig
                  return $ pluginDir cfg </> b


{- This lets you build custom external plugins
 - to handle specific block classes. That way you don't
 - have to write the 'bin' over and over. 
 - It works the same way as above except you're
 - hard-coding the attributes.
 -
 - For example, this is a complete CustomPlugin.hs:
 -
 - > module CustomPlugin (plugin) where
 - > import Gitit.Network.Plugin.External (mkPlugin)
 - > plugin = mkPlugin "custom" "html" "/path/to/my/binary" ["uri"]
 -
 - And here's how you might use it:
 -
 - > ~~~ { .custom }
 - > gitit will run '/path/to/my/binary --uri <page uri>'
 - > this text will be sent as stdin
 - > and then stdout will be wrapped in a RawBlock "html"
 - > ~~~
 -}
mkPlugin :: String -> String -> FilePath -> [String] -> Plugin
mkPlugin cls fmt bin ask = mkPageTransformM tfm
  where
    tfm :: Block -> PluginM Block
    tfm (CodeBlock (_, cs, as) txt) | elem cls cs = do
      args <- argList ask as
      -- name <- liftIO $ getDataFileName bin
      name <- findBinary bin
      out  <- liftIO $ eval name args txt
      return $ wrap fmt out
    tfm x = return x

-- TODO can you get the raw page path from a Page object somewhere?
--      looks like it from the debug output...

uri2path :: String -> FilePath
uri2path uri
  = concat $ intersperse [pathSeparator]
  $ filter (/= "")                        -- remove blanks
  $ filter (\s -> not $ isPrefixOf "_" s) -- remove _edit etc.
  $ splitOn [pathSeparator] uri

-- returns the path to the .page file associated with a request
-- TODO should it find stuff in the ghc data dir too?
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
  case isPrefixOf "/" lnk of
    True  -> return $ joinPath [repositoryPath cfg, dropWhile (== '/') lnk]
    False -> return $ joinPath [takeDirectory pfp, lnk]
