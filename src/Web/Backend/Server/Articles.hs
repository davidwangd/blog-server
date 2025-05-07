{-# LANGUAGE OverloadedStrings #-}

module Web.Backend.Server.Articles
    ( handleArticles
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
        if (author article == userId user)
            then H.a ! A.href ("/editor/" ++ show (id article)) $ "Edit"
            else toHtml ""
        H.a ! A.href ("/view_article/" ++ show (id article)) $ "View"

handleArticles :: ServerPart Response
handleArticles = do
    user <- getUser
    let lvl = case user of
        Just u -> level u
        Nothing -> 0
    conn <- lift openDB 
    articles <- lift $ (getAll conn):: [Article]
    ok $ toResponse $ addHeadTitle "Articles" $ articlePage (fromMaybe def user) articles