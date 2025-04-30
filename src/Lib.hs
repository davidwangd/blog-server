module Lib
    ( md2html
    , md2html'
    ) where

import Text.Pandoc
import qualified Data.Text as T
import Text.StringConvert 

md2html :: String -> IO String
md2html md = do
    res <- runIO $ readMarkdown def (T.pack md) >>= writeHtml5String def
    ret <- handleError res
    return (toString ret)

md2html' :: String -> String
md2html' md = case res of
    Left _ -> error "Parse Error"
    Right a -> (toString a)
    where res = runPure $ readMarkdown def (T.pack md) >>= writeHtml5String def 

