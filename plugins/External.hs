module External (plugin)
  where

-- import Control.Monad.IO.Class
-- import Data.Maybe
import Network.Gitit.Interface

{- This renders generic "external" codeblocks using whatever
 - command you want. The 'bin' attribute is required,
 - but 'fmt' defaults to "plain" (plain text).
 -
 - Other fmt options are:
 -   "para" for plain text as a paragraph
 -   "csv"  for a table of comma-separated values
 -   "list" for a bullet list
 -   "html" for raw html
 -
 - Here's how you might use it:
 -
 - > ~~~ { .external bin="/path/to/my/binary" fmt="html" }
 - > this text is sent through /path/to/my/binary
 - > and then wrapped in RawBlock "html"
 - > ~~~
 -}
plugin :: Plugin
plugin = mkPageTransformM tfm
  where
    tfm :: Block -> PluginM Block
    tfm (CodeBlock (_, cs, as) txt) | elem "external" cs = do
      args <- argList
      let bin = fromMaybe "" $ lookup "bin" as
          fmt = fromMaybe "" $ lookup "fmt" as
      out <- liftIO $ eval bin (flatten as ++ args) txt
      return $ wrap fmt out
    tfm x = return x
