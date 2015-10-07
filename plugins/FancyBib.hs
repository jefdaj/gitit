module FancyBib where

import Network.Gitit.Interface

import Control.Exception     (try, SomeException)
import Data.FileStore        (Resource(FSFile))
import Data.FileStore        (Resource(FSFile, FSDirectory), directory)
import Data.List             (isPrefixOf, intercalate)
import Data.Map              (insert)
import Data.Maybe            (mapMaybe)
import System.FilePath       (addExtension, splitExtension)
import System.FilePath       (takeBaseName)
import Text.CSL.Input.Bibtex (readBibtexInputString)
import Text.CSL.Pandoc       (processCites)
import Text.CSL.Parser       (readCSLFile)
import Text.CSL.Reference    (Reference, refId, title, unLiteral)
import Text.CSL.Style        (Style, unFormatted)


---------------
-- utilities --
---------------

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

-- TODO is this the same as askFiles?
listFiles :: FilePath -> PluginM [Resource]
listFiles dir = do
  fs  <- askFileStore
  res <- liftIO (try (directory fs dir) :: IO (Either SomeException [Resource]))
  case res of
    Left  _     -> return []
    Right files -> return files

-- TODO if you export this, don't need fileListToHtmlNoUplink?
render :: String -> String -> [Resource] -> String
render prefix ext rs = show $ fileListToHtmlNoUplink "" prefix ext rs

renderFiles :: String -> String -> String -> [String] -> [Block]
renderFiles _ _ _ [] = []
renderFiles t p e fs = [header, html]
  where
    rs     = map FSFile fs
    html   = RawBlock (Format "html") (render p e rs)
    header = Para [Str t]


--------------
-- citeproc --
--------------

isBibBlock :: Block -> Bool
isBibBlock (CodeBlock (_,cs,_) _) = "bib" `elem` cs
isBibBlock _ = False

extractRefs :: Pandoc -> (Pandoc, Maybe String)
extractRefs (Pandoc meta blks) = (Pandoc meta blks', bib')
  where
    blks' = filter (not . isBibBlock) blks
    bib   = filter isBibBlock blks
    bib'  = if null bib
              then Nothing
              else Just $ blocksToString bib

-- starting from Maybe String lets you treat all ref sources the same
parseRefs :: Maybe String -> PluginM [Reference]
parseRefs blks = do
  conf <- askConfig
  meta <- askMeta
  txt <- liftIO $ head $ mapMaybe id
    [ fmap return   $ blks
    , fmap readFile $ lookup "bibliography" meta
    , fmap readFile $ defaultBibliography   conf
    , fmap return   $ Just ""
    ]
  bib <- liftIO $ readBibtexInputString True txt
  return bib

getRefs :: Pandoc -> PluginM [Reference]
getRefs doc = do
  let (_, bib) = extractRefs doc
  bib' <- parseRefs bib
  return bib'

parseStyle :: PluginM Style
parseStyle = do
  cfg <- askConfig
  sty <- liftIO $ readCSLFile Nothing $ citationStyle cfg
  return sty

processCiteProc :: Pandoc -> PluginM Pandoc
processCiteProc doc = do
  let (doc', bib) = extractRefs doc
  bib' <- parseRefs bib
  sty  <- parseStyle
  let doc'' = processCites sty bib' doc'
  return doc''


--------------------
-- citeproc-title --
--------------------

-- I couldn't figure out getReference Locators, so I worked around them
keyAndTitle :: Reference -> (String, [Inline])
keyAndTitle r = (unId r, unTitle r)
  where
    unId    = unLiteral   . refId
    unTitle = unFormatted . title

-- TODO have just getThisRef, and exract the title afterward?
--      (so you can get the other stuff too maybe)
getThisTitle :: Pandoc -> PluginM (Maybe [Inline])
getThisTitle doc = do
  name <- askName
  refs <- getRefs doc
  let rmap = map keyAndTitle refs
  return $ lookup name rmap

setTitleIfNeeded :: Pandoc -> PluginM Pandoc
setTitleIfNeeded doc = do
  ref <- getThisTitle doc
  case ref of
    Nothing -> return doc
    Just r  -> return $ setTitle doc r


----------------
-- cite-links --
----------------

citeLinks :: Inline -> PluginM Inline
citeLinks s@(Str ('@':name)) = do
  page <- isPage     name
  this <- isThisPage name
  if page && (not this)
    then return $ Link [Str name] (name, name)
    else return s
citeLinks x = return x


-------------------
-- related-files --
-------------------


-- TODO don't assume .page
isRelatedTo :: FilePath -> FilePath -> Bool
isRelatedTo p p2 = (pb `isPrefixOf` p2b) && not (p2 `elem` [pb, pp])
  where
    base = fst . splitExtension
    pb  = base p
    pp  = addExtension pb ".page"
    p2b = base p2

-- TODO figure out the default extensions thing!
processRelated :: Pandoc -> PluginM Pandoc
processRelated (Pandoc m bs) = do
  n <- askName
  r <- askRequest
  let d = reqDir r
  fs <- listFiles d
  let ss = filter (isRelatedTo n) (map resPath fs)
      rs = renderFiles "Related files:" (d ++ "/") "page" ss
  return $ Pandoc m (rs ++ bs)


----------
-- main --
----------

plugin :: Plugin
plugin = citeLinks >>= citeProc >>= cpTitle >>= addRel
  where
    citeProc  = PageTransform processCiteProc
    cpTitle   = PageTransform setTitleIfNeeded
    citeLinks = mkPageTransformM citeLinks
    addRel    = PageTransform processRelated
