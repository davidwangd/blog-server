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
import Web.Backend.Utils

previewArticle :: User -> Article -> H.Html
previewArticle user post = 
    H.article ! A.class_ "bg-white rounded-xl shadow-md overflow-hidden transition-all duration-300 hover:shadow-xl" $ do
        H.div ! A.class_ "p-6" $ do
            H.h2 ! A.class_ "text-xl font-bold text-gray-800 mb-2 hover:text-primary transition-colors duration-300" $ do
                H.a ! A.href (H.stringValue $ "/view_article/" ++ show (getId post)) $ toHtml (title post)
        H.div ! A.class_ "flex items-center text-sm text-gray-500 mb-4" $ do
            H.span ! A.class_ "flex items-center mr-4" $ do
                H.i ! A.class_ "fa fa-user mr-1" $ ""
                toHtml (author post)
            H.span ! A.class_ "flex items-center" $ do
                H.i ! A.class_ "fa fa-calendar mr-1" $ ""
                toHtml (show $ createdAt post)
        H.p ! A.class_ "text-gray-600 mb-4 line-clamp-3" $ toHtml ((take 140 $ T.unpack $ content post) ++ " ... ")
        H.a ! A.class_ "inline-flex items-center text-primary font-medium hover:text-primary/80 transition-colors duration-300" 
            ! A.href (H.stringValue $ "/view_article/" ++ show (getId post)) $ do
            "阅读更多"
            H.i ! A.class_ "fa fa-arrow-right ml-1" $ ""
            if (getId user == author post || level user >= 4)
                then do
                    H.br
                    H.a ! A.class_ "text-blue-500 hover:text-blue-700 transition-colors duration-300" 
                        ! A.href (H.stringValue $ "/editor/" ++ show (getId post)) $ do
                        H.i ! A.class_ "fa fa-edit ml-2" $ mempty
                        toHtml (" 编辑" :: String)
                    H.a ! A.class_ "text-red-500 hover:text-red-700 transition-colors duration-300" 
                        ! A.href (H.stringValue $ "/delete_article/" ++ show (getId post)) $ do
                        H.i ! A.class_ "fa fa-trash ml-2" $ mempty
                        toHtml (" 删除" :: String)
                else mempty

articlePage :: User -> [Article] -> H.Html
articlePage user articles = do
    H.div ! A.class_ "bg-gray-50 font-inter text-dark min-h-screen flex flex-col" $ do
        -- 导航栏
        H.header ! A.class_ "bg-white border-b border-gray-200 shadow-sm sticky top-0 z-50 transition-all duration-300" $ do
            H.div ! A.class_ "container mx-auto px-4 py-3 flex justify-between items-center" $ do
                H.div ! A.class_ "flex items-center space-x-2" $ do
                    H.i ! A.class_ "fa fa-feather text-primary text-2xl" $ ""
                
                H.nav ! A.class_ "hidden md:flex space-x-6" $ do

                    if (getId user /= -1) 
                        then do
                            H.div ! A.class_ "text-gray-700 font-medium" $ toHtml ("Hello~ " ++ T.unpack (username user))
                            H.a ! A.class_ "text-green-700 hover:text-primary transition-colors duration-300 font-medium" ! A.href "/editor/nnew" $ "新文章"
                        else mempty
                    H.a ! A.class_ "text-gray-700 hover:text-primary transition-colors duration-300 font-medium" ! A.href "/" $ "首页"
                    H.a ! A.class_ "text-gray-700 hover:text-primary transition-colors duration-300 font-medium" ! A.href "/about" $ "关于"
                    H.a ! A.class_ "text-gray-700 hover:text-primary transition-colors duration-300 font-medium" ! A.href "/contact" $ "联系"
                    if (getId user == -1) then
                        H.a ! A.class_ "text-green-700 hover:text-primary transition-colors duration-300 font-medium" ! A.href "/login" $ "登录"
                    else
                        H.a ! A.class_ "text-red-700 hover:text-primary transition-colors duration-300 font-medium" ! A.href "/logout" $ "登出"

                H.button ! A.class_ "md:hidden text-gray-700 focus:outline-none" ! A.id "menu-toggle" $ 
                    H.i ! A.class_ "fa fa-bars text-xl" $ ""
        
        -- 移动端菜单
        H.div ! A.class_ "md:hidden hidden bg-white border-t border-gray-200" ! A.id "mobile-menu" $ do
            H.div ! A.class_ "container mx-auto px-4 py-2 space-y-3" $ do
                H.a ! A.class_ "block py-2 text-gray-700 hover:text-primary transition-colors duration-300" ! A.href "/" $ "首页"
                H.a ! A.class_ "block py-2 text-gray-700 hover:text-primary transition-colors duration-300" ! A.href "/about" $ "关于"
                H.a ! A.class_ "block py-2 text-gray-700 hover:text-primary transition-colors duration-300" ! A.href "/contact" $ "联系"
        
        -- 主内容
        H.main ! A.class_ "flex-grow container mx-auto px-4 py-8" $ do
            H.div ! A.class_ "max-w-6xl mx-auto" $ do
                -- 标题部分
                H.section ! A.class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6" $ do
                    mapM_ (previewArticle user) articles 

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