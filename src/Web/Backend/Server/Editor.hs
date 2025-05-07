{-# LANGUAGE OverloadedStrings #-}

module Web.Backend.Server.Editor
    ( editor
    ) where

import Web.Backend.Auth
import Web.Backend.Data
import Web.Frontend
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad (msum, liftM)
import Control.Monad.Trans (lift)
import Safe

editorPage :: User -> Article -> H.Html
editorPage user article = do
    H.div ! A.class_ "editor-container" $ do
        H.h1 "Editor"
        H.form ! A.method "POST" ! A.action ("/editor/" ++ name) $ do
            H.label "Title:"
            H.input ! A.type_ "text" ! A.name "title" ! A.value (TL.toStrict $ title article)
            H.label "Content:"
            H.textarea ! A.name "contents" $ toHtml (content article)
            H.input ! A.type_ "submit" ! A.value "Submit"
    
editor :: ServerPart Response
editor = msum 
    [ path editorGET
    , path editorPOST
    ]

getArticle :: String -> ServerPart (Maybe Article)
getArticle name = do
    conn <- lift getConn
    let tid = (readMay name) :: Maybe Int
    case tid of
        Just id -> return headMay $ queryById id conn
        Nothing -> return Nothing

editorGET :: String -> ServerPart Response
editorGET name = method GET >> verifyUserLevel
    [ seeOther ("/login") $ toResponse "Please login first"
    , seeOther ("/login") $ toResponse "You don't have permission to access this page"
    , do
        user <- getUser
        article <- getArticle name
        ok $ toResponse $ addHeadTitle "编辑" $ editorPage user (fromMaybe def article)
    ]

editorPOST :: String -> ServerPart Response
editorPOST name = method POST >> verifyUserLevel 
    [ ok $ toResponse $ "Noth authed"
    , ok $ toResponse $ "Noth authed"
    , do
        contents <- lookText "contents"
        title <- lookText "title"
        curTime <- liftIO getCurrentTime
        user <- getUser
        let tid = (readMay name) :: Maybe Int
        let arti = def :: Article
                { articleId = fromMaybe (-1) tid
                , author = userId user
                , updatedAt = curTime
                , title = title
                , content = contents
                , hasUrl = False
                , articleAccessiblity = 0
                }
        case tid of
            Just id -> do
                -- TODO: Update the database with the new content
                let newA = arti { articleId = id
                                }
            Nothing -> do
                -- TODO: New Content
                let newA = arti { articleId = -1
                               , createdAt = curTime
                               }
                conn <- lift getConn
                insert newA conn
                seeOther ("/article/" ++ show (articleId newA)) $ toResponse "Article Created"
    ]