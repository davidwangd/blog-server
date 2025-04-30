{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Happstack.Server
import Control.Monad (msum)

import Web.Frontend.Template
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad.IO.Class (liftIO)

showFile :: FilePath -> IO H.Html
showFile filepath = TIO.readFile filepath >>= renderMarkdown 

stylesPage :: ServerPart Response
stylesPage = serveDirectory EnableBrowsing ["index.html"] "submodules/github-markdown-css"

homepage :: ServerPart Response
homepage = do
    html <- liftIO $ showFile "about.md"
    ok $ toResponse $ addHeadTitle "HomePage" $ html

myApp :: ServerPart Response
myApp = msum 
    [ dir "styles" $ stylesPage
    , dir "sources" $ serveDirectory DisableBrowsing [] "public/sources"
    , homepage
    ]

main :: IO ()
main = do
    putStrLn $ "visit http://localhost:" ++ (show servingPort) ++ " or see Styles at http://localhost:" ++ (show servingPort) ++ "/styles"
    simpleHTTP (nullConf { port = servingPort }) myApp
    where servingPort = 8000