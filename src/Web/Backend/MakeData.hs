{-# LANGUAGE TemplateHaskell #-}
module Web.Backend.MakeData
    ( DBTable(..)
    , makeDBInstance
    ) where

import Language.Haskell.TH
import Data.Char
import Data.List.Split

class DBTable a where
    tableName :: a -> String
    -- createTableStmt :: a -> String
    -- insert :: a -> Connection -> IO ()
    -- queryById :: Int -> Connection -> IO a
    -- all :: Connection -> IO [a]
    -- removeById :: a -> Connection -> IO ()

makeDBInstance :: Name -> DecsQ
makeDBInstance name = do
    return $ [ InstanceD Nothing [] (AppT (ConT $ mkName "DBTable") (ConT name)) decs ]
    where decs = [ FunD (mkName "tableName") [Clause [VarP $ mkName "_"] (NormalB tbName) []] ]
          tbName = LitE $ StringL $ (map toLower $ last $ splitOn "." $ show $ name)
