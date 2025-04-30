{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Happstack.Server
import Control.Monad (msum)

import Web.Frontend.Template
import qualified Data.Text as T

about :: T.Text
about = T.pack $ "# About\n\n" ++
    "一个简易的渲染\n\n" ++
    "> 引用测试\n\n" ++
    "+ 列表元素1\n" ++
    "+ 列表元素2\n" 

stylesPage :: ServerPart Response
stylesPage = serveDirectory EnableBrowsing ["index.html"] "submodules/github-markdown-css"

homepage :: ServerPart Response
homepage = ok $ toResponse $ addHeadTitle "HomePage" $ markdownWrapper $ renderMarkdown' about

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