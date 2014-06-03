module Network.Gitit.Plugin.External (plugin, mkPlugin, Plugin, eval, wrap, flatten, argList)
  where

-- TODO get pluginDir and append to the string mkPlugin takes
-- TODO fix how it's not passing args at all!

import Control.Monad.IO.Class
import Data.Maybe
import Network.Gitit.Interface
import System.FilePath
import Text.Pandoc.Definition
import System.Exit
import System.Process


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


flatten :: [(String, String)] -> [String]
flatten = concat . map flagify
  where
    flagify (k, v) = ["--" ++ k, v]


-- asks for the plugin data available from gitit and
-- formats it as command line args for external scripts
argList :: PluginM [String]
argList = do
  c <- askConfig
  m <- askMeta
  r <- askRequest
  return $ concat [flatten m, cfgFlags c, reqFlags r]
  where
    reqFlags r = flatten [("uri", rqUri r)]
    cfgFlags c = flatten
      [ ("repository-path", repositoryPath c)
      , ("templates-dir"  , templatesDir   c)
      , ("static-dir"     , staticDir      c)
      , ("cache-dir"      , cacheDir       c)
      ]


{- This renders generic "external" codeblocks using whatever
 - command you want. The 'bin' attribute is required,
 - but 'fmt' defaults to "plain" (plain text).
 -
 - Other fmt options are:
 -   "para" for plain text as a paragraph
 -   "csv"  for a table of comma-separated values
 -   "list" for a bullet list
 -   "html" for raw html
 -
 - Here's how you might use it:
 -
 - > ~~~ { .external bin="/path/to/my/binary" fmt="html" }
 - > this text is sent through /path/to/my/binary
 - > and then wrapped in RawBlock "html"
 - > ~~~
 -}
plugin :: Plugin
plugin = mkPageTransformM tfm
  where
    tfm :: Block -> PluginM Block
    tfm (CodeBlock (_, cs, as) txt) | elem "external" cs = do
      args <- argList
      let bin = fromMaybe "" $ lookup "bin" as
          fmt = fromMaybe "" $ lookup "fmt" as
      out <- liftIO $ eval bin (flatten as ++ args) txt
      return $ wrap fmt out
    tfm x = return x


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
 - > plugin = mkPlugin "custom" "html" "/path/to/my/binary"
 -
 - And here's how you might use it:
 -
 - > ~~~ { .custom }
 - > text here will be piped through /path/to/my/binary
 - > and then wrapped in a RawBlock "html"
 - > ~~~
 -}
mkPlugin :: String -> String -> FilePath -> Plugin
mkPlugin cls fmt bin = mkPageTransformM tfm
  where
    tfm :: Block -> PluginM Block
    tfm (CodeBlock (_, cs, as) txt) | elem cls cs = do
      args <- argList
      out <- liftIO $ eval bin (flatten as ++ args) txt
      return $ wrap fmt out
    tfm x = return x
