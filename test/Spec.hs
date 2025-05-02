{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import Lib
import Web.Backend.Data
import Data.Text (Text, pack)
import Database.SQLite.Simple
import System.Process

main :: IO ()
main = hspec $ do
    describe "Test Convert" $ do
        it "header" $ do
            htmlString <- md2html "# h1"
            htmlString `shouldBe` "<h1>h1</h1>"

        it "link" $ do
            md2html' "[base](url)" `shouldBe` "<p><a href=\"url\">base</a></p>"
            md2html' "![name](url)" `shouldBe` "<p><img src=\"url\" alt=\"name\" /></p>"

    describe "Test Template Haskell" $ do
        
        it "Predefined TableName" $ do
            tableName (def :: User) `shouldBe` "user"
            tableName (def :: UserInfo) `shouldBe` "user_info"
            tableName (def :: Resource) `shouldBe` "resource"
            tableName (def :: Article) `shouldBe` "article"

        it "Predefined GetId" $ do
            getId (def :: User) `shouldBe` -1
            getId (def :: UserInfo) `shouldBe` -1
            getId (def :: Resource) `shouldBe` -1
            getId (def :: Article) `shouldBe` -1

        it "Predefined Fields" $ do
            fields (def :: User) `shouldBe` [("userId","Int"),("name","Text"),("level","Int")]
            fields (def :: UserInfo) `shouldBe` [("userInfoId","Int"),("username","Text"),("password","Text")] 
            fields (def :: Resource) `shouldBe` [("resourceId","Int"),("uploader","Int"),("url","Text"),("localPath","Text"),("accessiblity","Int")]
            fields (def :: Article) `shouldBe` [("articleId","Int"),("author","Int"),("createdAt","Text"),("updatedAt","Text"),("title","Text"),("content","Text"),("hasUrl","Bool")]

        it "SQL Create Stmt" $ do
            fromQuery (createTableStmt (def :: User)) `shouldBe` "CREATE TABLE IF NOT EXISTS user ( id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL,level INTEGER NOT NULL)"
            fromQuery (createTableStmt (def :: UserInfo)) `shouldBe` "CREATE TABLE IF NOT EXISTS userinfo ( id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, username TEXT NOT NULL,password TEXT NOT NULL)"
            fromQuery (createTableStmt (def :: Article)) `shouldBe` (pack $ "CREATE TABLE IF NOT EXISTS article ( id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, "
                ++"author INTEGER NOT NULL,created_at TEXT NOT NULL,updated_at TEXT NOT NULL,title TEXT NOT NULL,content TEXT NOT NULL,has_url BOOLEAN NOT NULL)")
        -- it "Predefined Fields" $ do
        --     fields (def :: User) `shouldBe` [("", "")]
        --     fields (def :: UserInfo) `shouldBe` [("", "")]
        --     fields (def :: Article) `shouldBe` [("", "")]
        --     fields (def :: Resource) `shouldBe` [("", "")]

        it "DataFields" $ do
            (dataFields (def { userId = 5, name = "Hello", level = 10 } :: User)) `shouldBe` toRow (("Hello", 10) :: (Text, Int))
            
        