-- {-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Happstack.Server
import Control.Monad (msum)

homepage :: ServerPart Response
homepage = ok $ toResponse "Welcome to the homepage!"

myApp :: ServerPart Response
myApp = msum 
    [ dir "hello" $ ok $ toResponse "Hello from /hello"
    , homepage
    ]

main :: IO ()
main = do
    putStrLn $ "visit http://localhost:" ++ (show servingPort) ++ " or see Hello at http://localhost:" ++ (show servingPort) ++ "/hello"
    simpleHTTP (nullConf { port = servingPort }) myApp
    where servingPort = 8000