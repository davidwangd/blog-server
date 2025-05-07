module EnvReader 
    ( readEnv 
    , readEnv'
    ) where

import Data.List.Split
import System.Environment
import Data.Default

findArgs :: String -> [String] -> Maybe String
findArgs x [] = Nothing
findArgs x (y:ys) = if ("--" ++ x ++ "=") == take (length x + 3) y
                        then Just (drop (length x + 3) y)
                        else findArgs x ys

readEnv :: (Read a) => String -> a -> IO a
readEnv envName df = do
    args <- getArgs
    case findArgs envName args of
        Just value -> return (read value)
        Nothing -> do
            envValue <- lookupEnv envName
            case envValue of
                Just value -> return (read value)
                Nothing -> do
                    envFiles <- readFile ".env"
                    let envLines = lines envFiles
                        envPairs = map ((\[k,v]->(k,v)) . splitOn "=") envLines
                    case lookup envName envPairs of
                        Just value -> return (read value)
                        Nothing -> return df

readEnv' :: (Read a, Default a) => String -> IO a
readEnv' envName = readEnv envName def
