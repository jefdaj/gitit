module DepGraph (plugin) where

import Network.Gitit.Interface       (Plugin)
import Network.Gitit.Plugin.External (mkPlugin)

-- TODO get plugin dir from gitit rather than hardcoding!
-- TODO rename mkPlugin to something more descriptive, like mkExternalPageTransform
plugin :: Plugin
plugin = mkPlugin
  "depgraph"
  "html"
  "/git/github/gitit/plugins/depgraph.py"
