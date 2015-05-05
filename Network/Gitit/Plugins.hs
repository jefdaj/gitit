{-# LANGUAGE CPP #-}
{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- Functions for loading plugins.
-}

module Network.Gitit.Plugins ( loadPlugins )
where

import Network.Gitit.Types
import qualified Network.Gitit.Plugin.Bibtex   as Bibtex
import qualified Network.Gitit.Plugin.Csv      as Csv
import qualified Network.Gitit.Plugin.Dot      as Dot
import qualified Network.Gitit.Plugin.External as External
import qualified Network.Gitit.Plugin.Files    as Files

loadPlugins :: [Plugin]
loadPlugins =
  [ Bibtex.plugin
  , Csv.plugin
  , Dot.plugin
  , External.plugin
  , Files.plugin
  ]
