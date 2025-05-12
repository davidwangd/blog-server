{-# LANGUAGE OverloadedStrings #-}

module Web.Backend.Server.Editor
    ( handleEditor
    ) where

import Web.Backend.Auth
import Web.Backend.Data
import Web.Backend.Sql
import Web.Frontend
import qualified Data.Text as T
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad (msum, liftM)
import Control.Monad.Trans (lift)
import Safe
import Happstack.Server 
import Text.Blaze.Html5 ((!), toHtml)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)

import Debug.Trace (trace)

editorPage :: String -> User -> Article -> H.Html
editorPage name1 user article = do
    H.div ! A.class_ "editor-container" $ do
        H.h1 "Editor"
        H.form ! A.method "POST" ! A.action (H.stringValue $ "/editor/" ++ name1) $ do
            H.label "Title:"
            H.input ! A.type_ "text" ! A.name "title" ! A.value (H.stringValue . T.unpack $ title article)
            H.label "Content:"
            H.textarea ! A.name "contents" $ toHtml (content article)
            H.input ! A.type_ "submit" ! A.value "Submit"
    
handleEditor :: ServerPart Response
handleEditor = msum 
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
    [ seeOther (T.pack "/login") $ toResponse ("Please login first" :: T.Text)
    , seeOther (T.pack "/login") $ toResponse ("You don't have permission to access this page" :: T.Text)
    , do
        user <- getUser
        article <- getArticle name
        lift $ putStrLn $ "user = " ++ show user ++ " , article = " ++ show article ++ " , name = " ++ name
        ok $ toResponse $ addHeadTitle "编辑" $ editorPage name (fromMaybe def user) (fromMaybe def article)
    ]

editorPOST :: String -> ServerPart Response
editorPOST name1 = method POST >> verifyUserLevel 
    [ ok $ toResponse $ ("Noth authed" :: T.Text)
    , ok $ toResponse $ ("Noth authed" :: T.Text)
    , do
        decodeBody (defaultBodyPolicy "/tmp" 0 1000 1000)
        contents <- liftM toStrict $ lookText "contents"
        title <- liftM toStrict $ lookText "title"
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
                seeOther (T.pack $ "/article/" ++ show id) $ toResponse ("Article Updated" :: T.Text)
            Nothing -> do
                -- TODO: New Content
                let newA = arti { articleId = -1
                               , createdAt = T.pack $ show curTime
                               }
                conn <- lift openDB 
                lift $ insert newA conn
                lift $ putStrLn $ "New article created: " ++ (show newA)
                seeOther (T.pack $ "/article/" ++ show (articleId newA)) $ toResponse (T.pack "Article Created")
    ]