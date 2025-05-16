module EnvReader 
    ( readEnv 
    , readEnv'
    ) where

import Data.List.Split
import System.Environment
import Data.Default
import Debug.Trace (trace)
import GHC.Stack
import Safe

findArgs :: String -> [String] -> Maybe String
findArgs x [] = Nothing
findArgs x (y:ys) = if ("--" ++ x ++ "=") == take (length x + 3) y
                        then Just (drop (length x + 3) y)
                        else findArgs x ys

readEnv :: (Read a, HasCallStack) => String -> a -> IO a
readEnv envName df = do
    args <- getArgs
    case findArgs envName args of
        Just value -> return (read value)
        Nothing -> do
            envValue <- lookupEnv envName
            case envValue of
                Just value -> case readMay value of
                    Just v -> return v
                    Nothing -> return df 
                Nothing -> do
                    envFiles <- readFile ".env"
                    let envLines = lines envFiles
                        envPairs = map ((\[k,v]->(k,v)) . splitOn "=") envLines
                    case lookup envName envPairs of
                        Just value -> case readMay value of
                            Just v -> return v
                            Nothing -> trace ("No parsing " ++ value ++ " Callof " ++ prettyCallStack callStack) $ return df 
                        Nothing -> return df

readEnv' :: (Read a, Default a, HasCallStack) => String -> IO a
readEnv' envName = readEnv envName def
