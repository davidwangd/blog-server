module Main (main) where

import Happstack.Server
import Web.Backend.Server
import Control.Monad
import Web.Backend.Sql (initDB)

main :: IO ()
main = do
    initDB
    putStrLn $ "Serving at " ++ "http://localhost:8000"
    simpleHTTP nullConf {port = 8000} $ msum
        [ dir "login" handleLogin
        , dir "register" handleRegister
        , homepage
        ]