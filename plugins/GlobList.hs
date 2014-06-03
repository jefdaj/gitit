module GlobList (plugin) where

import Network.Gitit.Interface       (Plugin)
import Network.Gitit.Plugin.External (mkPlugin)

{- Generates an HTML file listing like a page index,
 - except that 1) it's not a whole page, and 2) it filters
 - the listings.
 -}
plugin :: Plugin
plugin = mkPlugin
  "globlist"
  "html"
  "/git/github/gitit/plugins/globlist.py"
