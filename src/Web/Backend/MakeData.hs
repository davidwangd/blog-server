{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Web.Backend.MakeData
    ( DBTable(..)
    , makeDBInstance
    , FromRow
    , ToRow
    , toRow
    , query
    , query_
    , execute_
    , execute
    , headMay
    , liftM
    , Query
    , Only(..)
    ) where

import Language.Haskell.TH
import Data.Char
import Data.List.Split
import Data.List (intercalate)
import Data.Default
import Data.Data
import GHC.Generics
import Database.SQLite.Simple
import Database.SQLite.Simple.ToRow
import Safe
import Control.Monad

typename2tbname :: String -> String
typename2tbname [] = []
typename2tbname (x:xs) = hsName2SqlName (toLower x : xs)

hsName2SqlName :: String -> String
hsName2SqlName [] = []
hsName2SqlName (x:xs) = if (isLower x) then x:(hsName2SqlName xs)
                                       else '_':(toLower x):(hsName2SqlName xs)

sqlName2HsName :: String -> String
sqlName2HsName [] = []
sqlName2HsName (x:xs) = if (x == '_') then (toUpper $ head xs):sqlName2HsName (tail xs)
                                      else x:sqlName2HsName xs

tpConvert :: String -> String
tpConvert "Int"     = "INTEGER"
tpConvert "Integer" = "INTEGER"
tpConvert "Float"   = "REAL"
tpConvert "Double"  = "REAL"
tpConvert "Text"    = "TEXT"
tpConvert "String"  = "TEXT"
tpConvert "Bool"    = "BOOLEAN"
tpConvert x         = "BLOB"

hsType2SqlType :: String -> String
hsType2SqlType tp = case splitOn " " tp of
    [f, t] -> if f == "Maybe" then tpConvert t
                              else tpConvert t ++ " NOT NULL"
    [t]    -> tpConvert t ++ " NOT NULL"

          
class DBTable a where
    tableName :: a -> String
    getId :: a -> Int
    createTableStmt :: a -> Query
    insert :: a -> Connection -> IO ()
    queryById :: Int -> Connection -> IO (Maybe a)
    getAll :: Connection -> IO [a]
    remove :: a -> Connection -> IO ()
    fields :: a -> [(String, String)] 
    dataFields :: a -> [SQLData]
    update :: a -> Connection -> IO ()

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
    
    x <- newName "x"
    conn <- newName "conn"

    let decs = [ FunD (mkName "tableName")       [Clause [VarP $ mkName "_"] (NormalB tbName) []] 
               , FunD (mkName "getId")           [Clause [] (NormalB $ VarE (mkName idFunc)) []]
               , FunD (mkName "fields")          [Clause [VarP $ mkName "_"] (NormalB $ ListE $ map (\(a,b) -> TupE [Just $ LitE $ StringL a, Just $ LitE $ StringL b]) fields' ) []]
               , FunD (mkName "createTableStmt") [Clause [VarP $ mkName "_"] (NormalB $ LitE $ StringL createStmt) []]
               , FunD (mkName "dataFields")      [Clause [VarP x] (NormalB $ AppE (VarE $ mkName "toRow") (TupE $ map (\(fn,_) -> Just $ AppE (VarE $ mkName fn) (VarE x) ) $ drop 1 fields')) []]
                 -- getAll conn = query_ conn "SELECT * FROM ... "
               , FunD (mkName "getAll")          [Clause [VarP conn] (NormalB $ (VarE $ mkName "query_") `AppE` (VarE conn) `AppE` (LitE $ StringL selectStmt)) []]
                 -- insert x conn = execute conn "INSERT INTO ..." (dataFields x)
               , FunD (mkName "insert")          [Clause [VarP x, VarP conn] (NormalB $
                    (VarE $ mkName "execute") `AppE` (VarE conn) `AppE` (LitE $ StringL insertStmt) `AppE` ( (VarE $ mkName "dataFields") `AppE` (VarE x) ) ) []]
                 -- removeById x conn = execute conn "DELETE FROM ..." (Only $ idFunc x)
               , FunD (mkName "remove")          [Clause [VarP x, VarP conn] (NormalB $ 
                    (VarE $ mkName "execute") `AppE` (VarE conn) `AppE` (LitE $ StringL deleteStmt) `AppE` ( mkOnly (AppE (VarE $ mkName idFunc) $ VarE x)) ) []]
                 -- queryById x conn = liftM headMay $ query conn "SELECT * FROM ... WHERE " (Only x)
               , FunD (mkName "queryById")       [Clause [VarP x, VarP conn] (NormalB $
                    (VarE $ mkName "liftM") `AppE` (VarE $ mkName "headMay") `AppE` (
                        (VarE $ mkName "query") `AppE` (VarE conn) `AppE` (LitE $ StringL queryStmt) `AppE` ( mkOnly (VarE x) ) ) ) []]
                 -- update x conn = execute conn "UPDATE ..." (dataFields x, idFunc x)
               , FunD (mkName "update")         [Clause [VarP x, VarP conn] (NormalB $ 
                    (VarE $ mkName "execute") `AppE` (VarE conn) `AppE` 
                        (LitE $ StringL updateStmt) `AppE` (TupE $ map (\(fn, _)-> Just $ AppE (VarE $ mkName fn) (VarE x)) $ (drop 1 fields') ++ (take 1 fields')) ) []]   
               ]
        baseName   = base name
        tbName     = LitE $ StringL $ (typename2tbname baseName)
        idFunc     = (toLower (head baseName) : (drop 1 baseName)) ++ "Id"
        createStmt = "CREATE TABLE IF NOT EXISTS " ++ (map toLower baseName) ++ " ( id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, "
                     ++ (intercalate "," $ map toSqlField (drop 1 fields')) ++ ")"
        toSqlField = \(nm, tp) -> (hsName2SqlName nm) ++ " " ++ (hsType2SqlType tp)
        selectStmt = "SELECT * FROM " ++ (typename2tbname baseName)
        queryStmt  = selectStmt ++ " WHERE id = ?"
        -- INSERT INTO {tbName} (fields...) VALUES (?...)
        insertStmt = "INSERT INTO " ++ (typename2tbname baseName) ++ " (" 
                      ++ (intercalate "," $ map ( hsName2SqlName . fst ) $ drop 1 fields' ) ++ ") VALUES (" 
                      ++ (intercalate "," $ map (\_->"?") $ drop 1 fields') ++ ")"
        deleteStmt = "DELETE FROM " ++ (typename2tbname baseName) ++ " WHERE id = ?"
        updateStmt = "UPDATE " ++ (typename2tbname baseName) ++ " SET "
                     ++ (intercalate "," $ map (\(nm, _) -> (hsName2SqlName nm) ++ " = ?") $ drop 1 fields') 
                     ++ " WHERE id = ?"
        mkOnly     = \only -> (RecConE (mkName "Only") [(mkName "fromOnly", only)])
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