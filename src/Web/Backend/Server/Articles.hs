{-# LANGUAGE OverloadedStrings #-}

module Web.Backend.Server.Articles
    ( handleArticles
    , handleDeleteArticle
    ) where

import Web.Backend.Data
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Frontend
import Web.Backend.Auth
import Web.Backend.Sql
import Control.Monad.Trans (lift)
import Control.Monad (msum, liftM)
import Happstack.Server
import Data.Maybe (fromMaybe)
import Text.Blaze.Html5 ((!), toHtml)

articlePage :: User -> [Article] -> H.Html
articlePage user articles = do
    H.div ! A.class_ "article-container" $ do
        H.h1 "Articles"
        mapM_ (H.div ! A.class_ "article") $ map articleDiv articles
        H.a ! A.href "/editor/new" $ "Create New Article"
  where
    articleDiv article = do
        H.h2 ! A.class_ "article-title" $ toHtml (title article)
        H.p ! A.class_ "article-content" $ toHtml (content article)
        H.p ! A.class_ "article-author" $ toHtml (author article)
        H.span $ do
            if (author article == userId user)
                then H.input ! A.type_ "button" ! A.onclick (H.stringValue $ "window.location.href=/editor/" ++ show (getId article)) ! A.value "Edit"
                else toHtml (""::String)
        H.span $ do 
            H.input ! A.type_ "button" ! A.onclick (H.stringValue $ "window.location.href=/delete_article/" ++ show (getId article)) ! A.value "Delete"
        H.span $ do
            H.input ! A.type_ "button" ! A.onclick (H.stringValue $ "window.location.href=/view_article/" ++ show (getId article)) ! A.value "View"

handleArticles :: ServerPart Response
handleArticles = do
    user <- getUser
    let lvl = case user of
                Just u -> level u
                Nothing -> 0
    conn <- lift openDB 
    articles <- lift $ (getAll conn)
    ok $ toResponse $ addHeadTitle "Articles" $ articlePage (fromMaybe def user) articles

handleDeleteArticle :: String -> ServerPart Response
handleDeleteArticle aid = do
    method GET
    user <- liftM (fromMaybe def) getUser
    conn <- lift openDB 
    let articleId = read aid :: Int
    ariticle <- liftM (fromMaybe def) $ lift $ queryById articleId conn
    if author ariticle == userId user || level user >= 4
        then do
            lift $ remove ariticle conn
            seeOther (T.pack "/articles") (toResponse ())
        else do
            seeOther (T.pack "/articles") (toResponse ("You don't have permission to delete this article" :: T.Text))