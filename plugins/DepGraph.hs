module Network.Gitit.Plugin.DepGraph (plugin)
  where

import Network.Gitit.Plugin.External (mkPlugin)

-- TODO get plugin dir from gitit rather than hardcoding!
-- TODO rename mkPlugin to something more descriptive, like mkExternalPageTransform
plugin = mkPlugin "depgraph" "html" "/git/github/gitit/plugins/depgraph.py"
