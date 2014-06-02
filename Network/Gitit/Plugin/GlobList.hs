module Network.Gitit.Plugin.GlobList (plugin)
  where

-- import Network.Gitit.Interface
-- import Network.Gitit.Framework (isPageFile)
-- -- import Network.Gitit.Handlers (fileListToHtml)
-- import Network.Gitit.Plugin.External hiding (Plugin)
-- import System.FilePath
import Network.Gitit.Plugin.External (mkPlugin)

-- Generates an HTML file listing like a page index,
-- except that 1) it's not a whole page, and 2) it filters
-- the listings.

-- possibly good idea:
--   it takes finds files using python (for now)
--   then it makes them into a [Resource]
--   then it generates the index with fileListToHtml

plugin = mkPlugin
  "globlist"
  "html"
  "/git/github/gitit/plugins/globlist.py"

-- TODO I think getPath from Handlers should get the path of a page; use that!

-- TODO don't hardcode this!
-- bin :: String
-- bin = "/git/github/gitit-plugins/plugins/globlist.py"
-- 
-- makeResource :: FilePath -> Resource
-- makeResource f
  -- | isPageFile f = FSFile      f
  -- | otherwise    = FSDirectory f
-- 
-- plugin :: Plugin
-- plugin = mkPageTransformM tfm
  -- where
    -- tfm :: Block -> PluginM Block
    -- tfm (CodeBlock (_, cs, as) txt) | elem "globlist" cs = do
      -- out <- liftIO $ eval bin txt
      -- return $ wrap "html" out
    -- tfm x = return x
