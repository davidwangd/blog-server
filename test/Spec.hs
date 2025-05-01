{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import Lib
import Web.Backend.Data
import Data.Text (Text)

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
            tableName (def :: UserInfo) `shouldBe` "userinfo"
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
            fields (def :: Article) `shouldBe` [("articleId","Int"),("author","Int"),("createdAt","Text"),("updatedAt","Text"),("title","Text"),("content","Text"),("hasURL","Bool")]

        -- it "Predefined Fields" $ do
        --     fields (def :: User) `shouldBe` [("", "")]
        --     fields (def :: UserInfo) `shouldBe` [("", "")]
        --     fields (def :: Article) `shouldBe` [("", "")]
        --     fields (def :: Resource) `shouldBe` [("", "")]