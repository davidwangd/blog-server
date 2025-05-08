{-# LANGUAGE OverloadedStrings #-}

module Web.Backend.Server.ViewArticle
    ( handleViewArticle
    ) where

import Web.Backend.Data
import Web.Backend.Sql
import Web.Backend.Auth
import Web.Frontend
import Happstack.Server
import Control.Monad.Trans (lift)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!), toHtml)
import qualified Data.Text as T
import Safe

viewPage :: Article -> H.Html
viewPage article = do
    H.div ! A.class_ "article-view" $ do
        H.h1 ! A.class_ "article-title" $ toHtml (title article)
        H.p ! A.class_ "article-author" $ toHtml (author article)
        H.div ! A.class_ "article-content" $ toHtml (renderMarkdown' (content article))
        H.p ! A.class_ "article-date" $ toHtml (show (createdAt article))
        H.a ! A.href "/articles" $ "Back to Articles"

handleViewArticle :: String -> ServerPart Response
handleViewArticle name = do
    conn <- lift openDB
    let tid = (readMay name) :: Maybe Int
    case tid of
        Just id -> do
            article <- lift $ queryById id conn
            case article of
                Just art -> ok $ toResponse $ addHeadTitle "View Article" $ viewPage art
                Nothing -> notFound $ toResponse ("Article not found"::String)
        Nothing -> notFound $ toResponse ("Invalid article ID"::String)
