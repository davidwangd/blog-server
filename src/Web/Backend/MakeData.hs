{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Web.Backend.MakeData
    ( DBTable(..)
    , makeDBInstance
    , FromRow
    , ToRow
    ) where

import Language.Haskell.TH
import Data.Char
import Data.List.Split
import Data.Default
import Data.Data
import GHC.Generics

import Database.SQLite.Simple
class DBTable a where
    tableName :: a -> String
    getId :: a -> Int
    -- createTableStmt :: a -> String
    -- insert :: a -> Connection -> IO ()
    -- queryById :: Int -> Connection -> IO a
    -- all :: Connection -> IO [a]
    -- removeById :: a -> Connection -> IO ()
    fields :: a -> [(String, String)] 

makeDBInstance :: Name -> DecsQ
makeDBInstance name = do
    info <- reify name

    let showFields (a, _, ConT x) = (base a, base x)
        base                      = last . splitOn "." . show

    fields' <- case info of
        TyConI (DataD _ _ _ _ [RecC _ realFields] _) -> return $ map showFields realFields
        _ -> do
            loc <- location
            ( fail $ "Type Must be a well-defined Basic Type in " ++ show loc ) :: Q [(String, String)]
    

    let decs = [ FunD (mkName "tableName") [Clause [VarP $ mkName "_"] (NormalB tbName) []] 
               , FunD (mkName "getId")     [Clause [] (NormalB $ VarE (mkName idFunc)) []]
               , FunD (mkName "fields")    [Clause [VarP $ mkName "_"] (NormalB $ ListE $ map (\(a,b) -> TupE [Just $ LitE $ StringL a, Just $ LitE $ StringL b]) fields' ) []]
               ]
        baseName = base name
        tbName   = LitE $ StringL $ (map toLower baseName)
        idFunc   = (toLower (head baseName) : (tail baseName)) ++ "Id"

    return $ [ InstanceD Nothing [ -- AppT (ConT $ mkName "Data") (ConT name)
                                 -- , AppT (ConT $ mkName "Generic") (ConT name)
                                 {- , AppT (ConT $ mkName "Default") (ConT name) -} ] (AppT (ConT $ mkName "DBTable") (ConT name)) decs 
             , InstanceD Nothing [] (AppT (ConT $ mkName "FromRow") (ConT name)) []
             , InstanceD Nothing [] (AppT (ConT $ mkName "ToRow") (ConT name)) []]
    
    where 

{-
TyConI (
    DataD [] Web.Backend.Data.User [] Nothing 
        [RecC Web.Backend.Data.User 
            [(Web.Backend.Data.userId,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Int),
             (Web.Backend.Data.name,Bang NoSourceUnpackedness NoSourceStrictness,ConT Data.Text.Internal.Text),
             (Web.Backend.Data.level,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Int)
            ]
        ] 
[])
-}