{-# LANGUAGE OverloadedStrings #-}

module Web.Backend.Server.Editor
    ( editor
    ) where

import Web.Backend.Auth
import Web.Backend.Data
import Web.Backend.Sql
import Web.Frontend
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad (msum, liftM)
import Control.Monad.Trans (lift)
import Safe
import Happstack.Server 
import Text.Blaze.Html5 ((!), toHtml)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)

editorPage :: User -> Article -> H.Html
editorPage user article = do
    H.div ! A.class_ "editor-container" $ do
        H.h1 "Editor"
        H.form ! A.method "POST" ! A.action (H.stringValue $ "/editor/" ++ (T.unpack $ name user)) $ do
            H.label "Title:"
            H.input ! A.type_ "text" ! A.name "title" ! A.value (H.stringValue . T.unpack $ title article)
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
    conn <- lift openDB 
    let tid = (readMay name) :: Maybe Int
    case tid of
        Just id -> lift $ queryById id conn
        Nothing -> return Nothing

editorGET :: String -> ServerPart Response
editorGET name = method GET >> verifyUserLevel
    [ seeOther ("/login") $ toResponse "Please login first"
    , seeOther ("/login") $ toResponse "You don't have permission to access this page"
    , do
        user <- getUser
        article <- getArticle name
        ok $ toResponse $ addHeadTitle "编辑" $ editorPage (fromMaybe def user) (fromMaybe def article)
    ]

editorPOST :: String -> ServerPart Response
editorPOST name1 = method POST >> verifyUserLevel 
    [ ok $ toResponse $ "Noth authed"
    , ok $ toResponse $ "Noth authed"
    , do
        contents <- liftM LT.toStrict $ lookText "contents"
        title <- liftM LT.toStrict $ lookText "title"
        curTime <- lift getCurrentTime
        user <- getUser
        let tid = (readMay name1) :: Maybe Int
            arti = def 
                { articleId = fromMaybe (-1) tid
                , author = userId . fromMaybe def $ user
                , updatedAt = T.pack $ show curTime
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
                conn <- lift openDB 
                lift $ update newA conn
                seeOther ("/article/" ++ show id) $ toResponse "Article Updated"
            Nothing -> do
                -- TODO: New Content
                let newA = arti { articleId = -1
                               , createdAt = T.pack $ show curTime
                               }
                conn <- lift openDB 
                lift $ insert newA conn
                seeOther ("/article/" ++ show (articleId newA)) $ toResponse "Article Created"
    ]