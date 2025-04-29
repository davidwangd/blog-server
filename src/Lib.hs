module Lib
    ( md2html
    ) where

import Text.Pandoc
import qualified Data.Text as T
import Text.StringConvert 

md2html :: String -> IO String
md2html md = do
    res <- runIO $ do
        doc <- readMarkdown def (T.pack md)
        writeHtml5String def doc
    ret <- handleError res
    return (toString ret)


