{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Backend.Server.Editor
    ( handleEditor
    , handleSaveArticle
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
import Database.SQLite.Simple (lastInsertRowId)

import Debug.Trace (trace)


editorPage :: String -> User -> Article -> H.Html
editorPage name1 user article = do
    H.script ! A.src "/scripts/upload.js" $ mempty
    H.script ! A.src "/scripts/editor.js" $ mempty
    -- H.div ! A.onload "window.onload()" $ do
    --     H.h1 "Editor"
    --     H.form ! A.method "POST" ! A.action (H.stringValue $ "/editor/" ++ name1) $ do
    --         H.label "Title:"
    --         H.input ! A.type_ "text" ! A.name "title" ! A.value (H.stringValue . T.unpack $ title article)
    --         H.br
    --         H.label "Content:"
    --         H.textarea ! A.name "contents" $ toHtml (content article)
    --         H.input ! A.type_ "submit" ! A.value "Submit"
    --     H.br
    --     H.form ! A.id "upload-form" $ do
    --         H.label "Upload File:"
    --         H.input ! A.type_ "file" ! A.name "file"
    --         H.input ! A.type_ "submit" ! A.value "Upload"
    --         H.br
    --         H.label ! A.id "upload-result" $ "Upload Result: "

    H.div ! A.class_ "bg-gray-50 font-inter text-dark min-h-screen flex flex-col" $ do
        H.main ! A.class_ "flex-grow container mx-auto px-4 py-8" $ do
            H.div ! A.class_ "max-w-6xl mx-auto" $ do
                H.section ! A.class_ "mb-8" $ do
                    H.div ! A.class_ "bg-gray-50 rounded-t-lg p-3 border-b border-gray-200 flex justify-between items-center" $ do
                        H.div ! A.class_ "flex items-center space-x-2" $ do
                            H.div ! A.class_ "w-3 h-3 rounded-full bg-red-400" $ mempty
                            H.div ! A.class_ "w-3 h-3 rounded-full bg-yellow-400" $ mempty
                            H.div ! A.class_ "w-3 h-3 rounded-full bg-green-400" $ mempty
                            H.div ! A.class_ "ml-2 text-sm text-gray-500" $ mempty
                            H.span ! A.class_ "p-4 bg-white rounded-b-lg" $ "Content Editor"
                        H.div ! A.class_ "flex items-center space-x-2" $ do
                            H.button ! A.class_ "bg-blue-500 hover:bg-blue-700 text-white px-4 py-2 rounded" ! A.onclick (H.stringValue $ "rollback(" ++ show (getId article) ++ ");") $ "返回"
                            H.button ! A.class_ "bg-green-500 hover:bg-blue-700 text-white px-4 py-2 rounded" ! A.onclick (H.stringValue $ "new_subpage(" ++ show (getId article) ++ ");") $ "插入子页面"
                    H.div ! A.class_ "p-6" $ do
                        H.form ! A.id "editor-form" ! A.method "POST" ! A.action (H.stringValue $ "/save_article/" ++ name1) $ do
                            H.div ! A.class_ "mb-6" $ do
                                H.label ! A.for "title" ! A.class_ "block text-sm font-medium text-gray-700 mb-1" $ "Title:"
                                H.input ! A.type_ "text" ! A.id "title" ! A.name "title" ! A.class_ "w-full px-4 py-2 border border-gray-300 rounded-lg textarea-focus focus:outline-none"
                                    ! A.value (H.stringValue . T.unpack $ title article)
                            H.div ! A.class_ "mb-6" $ do
                                H.label ! A.for "contents" ! A.class_ "block text-sm font-medium text-gray-700 mb-1" $ "Content:"
                                H.textarea ! A.rows "20" ! A.id "contents" ! A.name "contents" ! A.class_ "w-full px-4 py-3 border border-gray-300 rounded-lg textarea-focus focus:outline-none resize-none font-mono" $ toHtml (content article)
                            H.div ! A.class_ "flex justify-between items-center" $ do
                                H.div ! A.class_ "flex items-center space-x-2" $ do
                                    H.label ! A.for "accessibility" ! A.class_ "p-4 block text-sm font-medium text-gray-700" $ "是否公开:"
                                    H.select ! A.id "accessibility" ! A.name "accessibility" ! A.class_ "p-4 border border-gray-300 rounded-lg textarea-focus focus:outline-none" $ do
                                        if (level user >= 3)
                                            then H.option ! A.value "0" $ "公开"
                                            else mempty
                                        H.option ! A.value "2" $ "登录用户"
                                        H.option ! A.value "4" $ "站主可见"
                                        H.option ! A.value "5" $ "私密"
                                H.button ! A.id "save_btn" ! A.type_ "submit" ! A.class_ "flex px-4 py-2 bg-primary hover:bg-primary/90 text-white rounded-lg font-medium btn-hover" ! A.action "submit()" $ do
                                    H.i ! A.class_ "fa fa-save mr-2" $ mempty
                                    toHtml ("Save" :: String)
                H.section $ do
                    H.div ! A.class_ "bg-white rounded-xl p-1 card-shadow" $ do
                        H.div ! A.class_ "bg-gray-50 rounded-t-lg p-3 border-b border-gray-200 flex items-center" $ do
                            H.div ! A.class_ "flex items-center space-x-2" $ do
                                H.div ! A.class_ "w-3 h-3 rounded-full bg-red-400" $ mempty
                                H.div ! A.class_ "w-3 h-3 rounded-full bg-yellow-400" $ mempty
                                H.div ! A.class_ "w-3 h-3 rounded-full bg-green-400" $ mempty
                                H.span ! A.class_ "ml-2 text-sm text-gray-500" $ "Upload File"
                    H.div ! A.class_ "p-6" $ do
                        H.form ! A.id "upload-form" ! A.class_ "space-y-4" $ do
                            H.div ! A.class_ "flex flex-col md:flex-row items-start md:items-center space-y-2 md:space-y-0" $ do
                                H.label ! A.for "file" ! A.class_ "text-sm font-medium text-gray-700" $ "Upload File:"
                                H.div ! A.class_ "flex-1 min-w-0" $ do
                                    H.div ! A.class_ "relative" $ do
                                        H.div ! A.class_ "flex absolute inset-y-0 left-0 items-center pl-3 pointer-events-none" $ do
                                            H.i ! A.class_ "fa fa-file text-gray-400" $ mempty
                                        H.input ! A.type_ "file" ! A.id "file" ! A.name "file" 
                                            ! A.class_ "block w-full pl-10 py-2 text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 cursor-pointer focus:outline-none focus:ring-2 focus:ring-primary/50 focus:border-primary"
                            H.div ! A.class_ "flex items-center" $ do
                                H.button ! A.type_ "submit" ! A.class_ "bg-secondary hover:bg-secondary/90 text-white px-6 py-2 rounded-lg font-medium btn-hover" $ do
                                    H.i ! A.class_ "fa fa-upload mr-2" $ mempty
                                    "Upload"
                                H.span ! A.id "upload-result" ! A.class_ "ml-4 text-sm text-gray-500" $ "Upload Result"                

handleEditor = editorView

getArticle :: String -> ServerPart (Maybe Article)
getArticle name = do
    conn <- lift openDB 
    let tid = (readMay name) :: Maybe Int
    case tid of
        Just id -> lift $ queryById id conn
        Nothing -> return Nothing

editorView :: String -> ServerPart Response
editorView name = verifyUserLevel
    [ seeOther (T.pack "/login") $ toResponse ("Please login first" :: T.Text)
    , seeOther (T.pack "/login") $ toResponse ("You don't have permission to access this page" :: T.Text)
    , if name == "new" || name == "new_sub"
        then do
            let sub = name == "new_sub"
                newTitle = if sub then "新建子页面" else "新建文章"
            user <- liftM (fromMaybe def) getUser
            conn <- lift openDB
            curTime <- lift getCurrentTime

            newContent <- if sub
                    then return ""
                    else do
                        decodeBody (defaultBodyPolicy "/tmp" 0 1000 1000)
                        preid <- liftM (T.unpack . toStrict) $ lookText "id"
                        return $ T.pack $ "[返回](/view_article/" ++ preid ++ ") <!-- 这是自动生成的返回上级目录链接， 可以保留，删除或者移动到期望位置 -->\n"
            let arti = def
                    { articleId = -1
                    , author = userId user
                    , updatedAt = T.pack $ show curTime
                    , createdAt = T.pack $ show curTime
                    , title = newTitle
                    , content = newContent
                    , hasUrl = sub
                    , articleAccessiblity = 0
                    }
            lift $ insert arti conn
            lid <- lift $ lastInsertRowId conn
            if name == "new" 
                then seeOther (T.pack $ "/editor/" ++ show lid) $ toResponse ()
                else ok $ toResponse (show lid)
        else do
            user <- getUser
            article <- getArticle name
            -- lift $ putStrLn $ "user = " ++ show user ++ " , article = " ++ show article ++ " , name = " ++ name
            ok $ toResponse $ addHeadTitle "编辑" $ editorPage name (fromMaybe def user) (fromMaybe def article)
    ]

handleSaveArticle :: String -> ServerPart Response
handleSaveArticle name1 = method POST >> verifyUserLevel 
    [ ok $ toResponse $ ("Noth authed" :: T.Text)
    , ok $ toResponse $ ("Noth authed" :: T.Text)
    , do
        decodeBody (defaultBodyPolicy "/tmp" 0 1000000 1000000)
        contents <- liftM toStrict $ lookText "contents"
        title <- liftM toStrict $ lookText "title"
        access <- liftM (T.unpack . toStrict) $ lookText "accessibility"
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
                , articleAccessiblity = read access
                }
        case tid of
            Just id -> do
                -- TODO: Update the database with the new content
                let newA = arti { articleId = id
                                }
                conn <- lift openDB 
                if (author arti == userId (fromMaybe def user) || level (fromMaybe def user) >= 4)
                    then do
                        lift $ update newA conn
                        seeOther (T.pack $ "/article/" ++ show id) $ toResponse ("Article Updated" :: T.Text)
                    else case user of
                        Just u -> seeOther (T.pack "/") $ toResponse ("You don't have permission to edit this article" :: T.Text)
                        Nothing -> seeOther (T.pack "/login") $ toResponse ("Please login first" :: T.Text)
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