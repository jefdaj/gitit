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

{- | Interface for plugins.

A plugin is a Haskell module that is dynamically loaded by gitit.

There are three kinds of plugins: 'PageTransform's,
'PreParseTransform's, and 'PreCommitTransform's. These plugins differ
chiefly in where they are applied. 'PreCommitTransform' plugins are
applied just before changes to a page are saved and may transform
the raw source that is saved. 'PreParseTransform' plugins are applied
when a page is viewed and may alter the raw page source before it
is parsed as a 'Pandoc' document. Finally, 'PageTransform' plugins
modify the 'Pandoc' document that results after a page's source is
parsed, but before it is converted to HTML:

>                 +--------------------------+
>                 | edited text from browser |
>                 +--------------------------+
>                              ||         <----  PreCommitTransform plugins
>                              \/
>                              ||         <----  saved to repository
>                              \/
>              +---------------------------------+
>              | raw page source from repository |
>              +---------------------------------+
>                              ||         <----  PreParseTransform plugins
>                              \/
>                              ||         <----  markdown or RST reader
>                              \/
>                     +-----------------+
>                     | Pandoc document |
>                     +-----------------+
>                              ||         <---- PageTransform plugins
>                              \/
>                   +---------------------+
>                   | new Pandoc document |
>                   +---------------------+
>                              ||         <---- HTML writer
>                              \/
>                   +----------------------+
>                   | HTML version of page |
>                   +----------------------+

Note that 'PreParseTransform' and 'PageTransform' plugins do not alter
the page source stored in the repository. They only affect what is
visible on the website.  Only 'PreCommitTransform' plugins can
alter what is stored in the repository.

Note also that 'PreParseTransform' and 'PageTransform' plugins will
not be run when the cached version of a page is used.  Plugins can
use the 'doNotCache' command to prevent a page from being cached,
if their behavior is sensitive to things that might change from
one time to another (such as the time or currently logged-in user).

You can use the helper functions 'mkPageTransform' and 'mkPageTransformM'
to create 'PageTransform' plugins from a transformation of any
of the basic types used by Pandoc (for example, @Inline@, @Block@,
@[Inline]@, even @String@). Here is a simple (if silly) example:

> -- Deprofanizer.hs
> module Deprofanizer (plugin) where
>
> -- This plugin replaces profane words with "XXXXX".
>
> import Network.Gitit.Interface
> import Data.Char (toLower)
>
> plugin :: Plugin
> plugin = mkPageTransform deprofanize
>
> deprofanize :: Inline -> Inline
> deprofanize (Str x) | isBadWord x = Str "XXXXX"
> deprofanize x                     = x
>
> isBadWord :: String -> Bool
> isBadWord x = (map toLower x) `elem` ["darn", "blasted", "stinker"]
> -- there are more, but this is a family program

Further examples can be found in the @plugins@ directory in
the source distribution.  If you have installed gitit using Cabal,
you can also find them in the directory
@CABALDIR\/share\/gitit-X.Y.Z\/plugins@, where @CABALDIR@ is the cabal
install directory and @X.Y.Z@ is the version number of gitit.

TODO add duplicated functions here as utilities:
  askFile
  askName
  link2path
  listFiles
  reqDir (that's somewhere already right?)
  render (but rename it)
  resPath?
  uri2path
-}

module Network.Gitit.Interface ( Plugin(..)
                               , PluginM
                               , mkPageTransform
                               , mkPageTransformM
                               , Config(..)
                               , Request(..)
                               , User(..)
                               , Context(..)
                               , PageType(..)
                               , PageLayout(..)
                               , askConfig
                               , askUser
                               , askRequest
                               , askFileStore
                               , askMeta
                               , doNotCache
                               , fileListToHtmlNoUplink
                               , getContext
                               , modifyContext
                               , inlinesToURL
                               , inlinesToString
                               , liftIO
                               , withTempDir
                               , module Text.Pandoc.Definition
                               , module Text.Pandoc.Generic

                               -- TODO clean these up and add Haddock docs
                               , blocksToString
                               , setTitle
                               , askName
                               , resPath
                               , isPage
                               , isThisPage
                               , reqDir
                               , listFiles
                               , render
                               , renderFiles
                               )
where
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Data.Data
import Network.Gitit.Types
import Network.Gitit.Handlers (fileListToHtmlNoUplink)
import Network.Gitit.ContentTransformer
import Network.Gitit.Util (withTempDir)
import Network.Gitit.Server (Request(..))
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM)
import Data.FileStore (FileStore)
import Data.Map (insert)
import System.FilePath (takeBaseName)
import Data.FileStore (Resource(FSFile, FSDirectory), directory)
import Data.List (intercalate)
import Control.Exception (try, SomeException)

-- | Returns the current wiki configuration.
askConfig :: PluginM Config
askConfig = liftM pluginConfig ask

-- | Returns @Just@ the logged in user, or @Nothing@ if nobody is logged in.
askUser :: PluginM (Maybe User)
askUser = liftM pluginUser ask

-- | Returns the complete HTTP request.
askRequest :: PluginM Request
askRequest = liftM pluginRequest ask

-- | Returns the wiki filestore.
askFileStore :: PluginM FileStore
askFileStore = liftM pluginFileStore ask

-- | Returns the page meta data
askMeta :: PluginM [(String, String)]
askMeta = liftM ctxMeta getContext

-- | Indicates that the current page or file is not to be cached.
doNotCache :: PluginM ()
doNotCache = modifyContext (\ctx -> ctx{ ctxCacheable = False })

-- | Lifts a function from @a -> a@ (for example, @Inline -> Inline@,
-- @Block -> Block@, @[Inline] -> [Inline]@, or @String -> String@)
-- to a 'PageTransform' plugin.
mkPageTransform :: Data a => (a -> a) -> Plugin
mkPageTransform fn = PageTransform $ return . bottomUp fn

-- | Monadic version of 'mkPageTransform'.
-- Lifts a function from @a -> m a@ to a 'PageTransform' plugin.
mkPageTransformM :: Data a => (a -> PluginM a) -> Plugin
mkPageTransformM =  PageTransform . bottomUpM

blocksToString :: [Block] -> String
blocksToString bs = unlines $ map (\(CodeBlock _ t) -> t) bs

-- because Text.Pandoc.Builder.setTitle doesn't work on a [Inline]
setTitle :: Pandoc -> [Inline] -> Pandoc
setTitle (Pandoc m bs) t = Pandoc m' bs
  where
    old = unMeta m
    new = insert "title" (MetaInlines t) old
    m'  = Meta {unMeta = new}

-- TODO is this available from the Interface already?
askName :: PluginM String
askName = do
  req <- askRequest
  let base = takeBaseName $ rqUri req
  return base

resPath :: Resource -> FilePath
resPath (FSFile      f) = f
resPath (FSDirectory d) = d

reqDir :: Request -> FilePath
reqDir = intercalate "/" . init . rqPaths

askFiles :: PluginM [FilePath]
askFiles = do
  fs  <- askFileStore
  req <- askRequest
  let dir = reqDir req
  res <- liftIO (try (directory fs dir) :: IO (Either SomeException [Resource]))
  case res of
    Left  _     -> return []
    Right files -> return $ map (takeBaseName . resPath) files

isPage :: FilePath -> PluginM Bool
isPage name = do
  files <- askFiles
  return $ name `elem` (map takeBaseName files)

isThisPage :: FilePath -> PluginM Bool
isThisPage name1 = do
  name2 <- askName
  return $ name1 == name2

listFiles :: FilePath -> PluginM [Resource]
listFiles dir = do
  fs  <- askFileStore
  res <- liftIO (try (directory fs dir) :: IO (Either SomeException [Resource]))
  case res of
    Left  _     -> return []
    Right files -> return files

-- TODO if you export this, don't need fileListToHtmlNoUplink?
render :: String -> [Resource] -> String
render prefix rs = show $ fileListToHtmlNoUplink "" prefix rs

renderFiles :: String -> String -> [String] -> [Block]
renderFiles _ _ [] = []
renderFiles t p fs = [header, html]
  where
    rs     = map FSFile fs
    html   = RawBlock (Format "html") (render p rs)
    header = Para [Str t]
