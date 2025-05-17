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
import Data.Maybe (fromMaybe)
import Safe
import Web.Backend.Utils

viewArticle :: Article -> H.Html
viewArticle article = H.docTypeHtml $ do
    H.head $ do
        addIcon
        H.title (toHtml $ title article)
        H.meta ! A.charset "utf-8"
        H.meta ! A.rel "stylesheet" ! A.href "/styles/github-markdown.css"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        H.script $ "MathJax = { tex: { inlineMath: [['\\(', '\\)'], ['$', '$']]  } };"
        H.script ! A.src "/sources/mathjax_settings.js" $ ""
        H.script ! A.type_ "text/javascript" ! A.async "" ! A.src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" $ ""
    H.body $ do
        H.div ! A.class_ "markdown-body" $ do
            H.h1 (toHtml $ title article)
            H.p (toHtml $ author article)
            H.p (toHtml $ (createdAt article))
            renderMarkdown' (content article)

handleViewArticle :: String -> ServerPart Response
handleViewArticle name = do
    conn <- lift openDB
    let tid = (readMay name) :: Maybe Int
    case tid of
        Just id -> do
            article <- lift $ queryById id conn
            case article of
                Just art -> do
                    ok $ toResponse $ viewArticle art
                Nothing -> notFound $ toResponse ("Article not found"::String)
        Nothing -> notFound $ toResponse ("Invalid article ID"::String)
