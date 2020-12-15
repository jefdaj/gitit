module Network.Gitit.Plugin.Dot (plugin) where

-- TODO: insert dot error messages rather than crashing!

-- This plugin allows you to include a graphviz dot diagram
-- in a page like this:
--
-- ~~~ {.dot name="diagram1"}
-- digraph G {Hello->World}
-- ~~~
--
-- The "dot" executable must be in the path.
-- The generated svg file will be cached in the cache directory.
-- A unique name will be generated from a hash of the file contents,
-- prefixed with the 'name' attribute if one is given.

import Data.Maybe (fromMaybe)
import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Monad (unless)

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "dot" `elem` classes = do
  cfg <- askConfig
  let (name, outfile) =  case lookup "name" namevals of
                                Just fn   -> ([Str fn], fn ++ ".png")
                                Nothing   -> ([], uniqueName contents ++ ".png")
  liftIO $ do
    (ec, _out, err) <- readProcessWithExitCode "dot" ["-Tpng", "-o",
                         staticDir cfg </> "img" </> outfile] contents
    let attr = ("image", [], [])
    if ec == ExitSuccess
       then return $ Para [Image attr name ("/img" </> outfile, "")]
       else error $ "dot returned an error status: " ++ err
transformBlock x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
