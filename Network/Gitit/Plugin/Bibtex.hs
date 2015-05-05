{-
Copyright (C) 2012 John Lenz <lenz@math.uic.edu>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list
of conditions and the following disclaimer.  Redistributions in binary form must
reproduce the above copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials provided with the
distribution.  Neither the name of John Lenz nor the names of its contributors
may be used to endorse or promote products derived from this software without
specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Changes by Jeff Johnson:

* remove redundant imports and variable names
* add (Format "html") as required by newer gitit
* remove mrNumber and arxiv functions
* make articleLink into urlLink
* add pdfLink to insert links to my pdf of the paper, if any
  (this required threading PluginM through some stuff)
-}

module Network.Gitit.Plugin.Bibtex where

-- For documentation, see
-- * http://blog.wuzzeb.org/posts/2012-06-19-bibtex-and-pandoc-2.html
-- * http://blog.wuzzeb.org/posts/2012-06-26-bibtex-and-gitit.html

import Control.Exception (try, SomeException)
import Control.Monad (liftM)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.FileStore (Resource(..), FileStore(..), directory)
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import Text.Pandoc
import System.FilePath ((</>))

import Network.Gitit.Interface

data Bibtex = Bibtex String [(String,String)]

bibParser :: P.Parser [Bibtex]
bibParser = do
  x <- P.sepEndBy bibEntry P.spaces
  P.eof
  return x

bibEntry :: P.Parser Bibtex
bibEntry = do
  P.char '@'
  P.many $ P.noneOf "{"
  P.char '{'
  name <- P.many $ P.noneOf ","
  P.many $ P.oneOf " \t\n,"
  attrs <- P.sepEndBy bibAttr $ P.many1 $ P.oneOf " \t\n,"
  P.char '}'
  return $ Bibtex name attrs

bibAttr :: P.Parser (String, String)
bibAttr = do
  key <- P.many (P.letter <|> P.digit)
  P.spaces
  P.char '='
  P.spaces
  P.char '{'
  val <- bibVal
  P.char '}'
  return (map toLower key, val)

bibVal :: P.Parser String
bibVal = liftM concat $ P.many1 (bibValMatched <|> (liftM (:[]) (P.noneOf "{}")))

bibValMatched :: P.Parser String
bibValMatched = P.between (P.char '{') (P.char '}') bibVal

type BibtexAttr = [(String, String)]

render1 :: BibtexAttr -> String -> Inline
render1 b s = case lookup s b of
    Just x  -> Str x
    Nothing -> Str ""

urlLink :: String -> BibtexAttr -> Inline
urlLink s b = case lookup s b of
                  Just x  -> Link [Str "url"] (x, [])
                  Nothing -> Str ""

pdfLink :: String -> PluginM Inline
pdfLink key = do
  req  <- askRequest
  stor <- askFileStore
  let dir  = reqDir req
      name = key ++ ".pdf"
  sdir <- liftIO (try (directory stor dir) :: IO (Either SomeException [Resource]))
  return $ case sdir of
    Left  _ -> Str ""
    Right d -> if (FSFile name) `elem` d
                 then Link [Str "pdf"] (name, [])
                 else Str ""

-- TODO deduplicate this with the one in Plugin/Files.hs
reqDir :: Request -> FilePath
reqDir = intercalate "/" . init . rqPaths

expandTex :: String -> String
expandTex ('\\':a:'{':b:'}':xs) = expandTex ('\\':a:b:xs)
expandTex ('\\':'\'':a:xs) = a' : expandTex xs
   where a' = case a of
               'a' -> 'á'
               'e' -> 'é'
               'o' -> 'ó'
               _   -> a
expandTex ('\\':'H':'o':xs) = 'ő' : expandTex xs
expandTex ('\\':'"':a:xs) = a' : expandTex xs
   where a' = case a of
               'a' -> 'ä'
               'e' -> 'ë'
               'o' -> 'ö'
               _   -> a
expandTex (a:xs) = a : expandTex xs
expandTex [] = []

prettyAuthor :: String -> String
prettyAuthor x = L.intercalate ", " $ map fixOne $ S.splitOn " and" x
  where fixOne s = case S.splitOn "," s of
                    []     -> ""
                    [a]    -> a
                    (f:xs) -> concat xs ++ " " ++ f

renderEntry :: String -> BibtexAttr -> PluginM [Inline]
renderEntry name b = do
  pdf <- pdfLink name
  let raw = [(RawInline (Format "html") $ "<a name=\"" ++ name ++ "\"></a>")]
      mapInline f (Str s) = Str $ f s
      mapInline _ x = x
      isEmptyStr (Str "") = True
      isEmptyStr _        = False
      entries = L.intersperse (Str ", ") $ filter (not . isEmptyStr)
        [ mapInline (prettyAuthor . expandTex) $ render1 b "author"
        , mapInline (\a -> "\"" ++ a ++ "\"") $ render1 b "title"
        , Emph [render1 b "journal"]
        , render1 b "year"
        , urlLink "url" b
        , urlLink "url2" b
        , pdf
        ]
  return $ raw ++ entries

renderEntries :: [Bibtex] -> PluginM Block
renderEntries lst = do
    let lst' = L.sortBy (\(Bibtex a _) (Bibtex b _) -> compare a b) lst
        display (Bibtex key b) = do
          entry <- renderEntry key b
          return $ ([Strong [Str key]], [map Plain [entry]])
    lst'' <- mapM display lst'
    return $ DefinitionList $ lst''

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, cs, _) txt) | "bib" `elem` cs = do
  let bs = P.parse bibParser "" txt
  case bs of
    Right bs' -> renderEntries bs'
    Left  err -> do
      let msg = "Error parsing bib data: " ++ show err
      return $ BlockQuote [Para [Str $ msg]]
transformBlock x = return x

plugin :: Plugin
plugin = mkPageTransformM transformBlock
