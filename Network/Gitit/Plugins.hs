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

module Network.Gitit.Plugins
  ( loadPlugin
  , loadPlugins
  , mkPlugin
  , argList -- TODO remove from exports
  , eval    -- TODO remove from exports
  , wrap    -- TODO remove from exports
  , flatten -- TODO remove from exports
  ) where

import Network.Gitit.Types
import System.FilePath
import Control.Monad (unless)
import System.Log.Logger (logM, Priority(..))
#ifdef _PLUGINS
import Data.List (isInfixOf, isPrefixOf)
import GHC
import GHC.Paths
import Unsafe.Coerce

import Text.Pandoc.Definition
import Network.Gitit.Interface
import System.Process
import System.Exit

loadPlugin :: FilePath -> IO Plugin
loadPlugin pluginName = do
  logM "gitit" WARNING ("Loading plugin '" ++ pluginName ++ "'...")
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    defaultCleanupHandler dflags $ do
      -- initDynFlags
      unless ("Network.Gitit.Plugin." `isPrefixOf` pluginName)
        $ do
            addTarget =<< guessTarget pluginName Nothing
            r <- load LoadAllTargets
            case r of
              Failed -> error $ "Error loading plugin: " ++ pluginName
              Succeeded -> return ()
      let modName =
            if "Network.Gitit.Plugin" `isPrefixOf` pluginName
               then pluginName
               else if "Network/Gitit/Plugin/" `isInfixOf` pluginName
                       then "Network.Gitit.Plugin." ++ takeBaseName pluginName
                       else takeBaseName pluginName
#if MIN_VERSION_ghc(7,4,0)
      pr <- parseImportDecl "import Prelude"
      i <- parseImportDecl "import Network.Gitit.Interface"
      m <- parseImportDecl ("import " ++ modName)
      setContext [IIDecl m, IIDecl  i, IIDecl pr]
#else
      pr <- findModule (mkModuleName "Prelude") Nothing
      i <- findModule (mkModuleName "Network.Gitit.Interface") Nothing
      m <- findModule (mkModuleName modName) Nothing
#if MIN_VERSION_ghc(7,2,0)
      setContext [IIModule m, IIModule i, IIModule pr] []
#elif MIN_VERSION_ghc(7,0,0)
      setContext [] [(m, Nothing), (i, Nothing), (pr, Nothing)]
#else
      setContext [] [m, i, pr]
#endif
#endif
      value <- compileExpr (modName ++ ".plugin :: Plugin")
      let value' = (unsafeCoerce value) :: Plugin
      return value'

#else

loadPlugin :: FilePath -> IO Plugin
loadPlugin pluginName = do
  error $ "Cannot load plugin '" ++ pluginName ++
          "'. gitit was not compiled with plugin support."
  return undefined

#endif

loadPlugins :: [FilePath] -> IO [Plugin]
loadPlugins pluginNames = do
  plugins' <- mapM loadPlugin pluginNames
  unless (null pluginNames) $ logM "gitit" WARNING "Finished loading plugins."
  return plugins'

---------------------------
-- stuff from External.hs
---------------------------

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
