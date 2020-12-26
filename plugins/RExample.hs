module RExample (plugin) where

import Network.Gitit.Interface         (Plugin)
import Network.Gitit.Plugins.External (mkPlugin, allArgs)

plugin :: Plugin
plugin = mkPlugin
  "rexample"   -- input block class
  "html"       -- output block class
  "rexample.R" -- name of script
  allArgs      -- list of args to pass
