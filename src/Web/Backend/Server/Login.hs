{-# LANGUAGE OverloadedStrings #-}

module Web.Backend.Server.Login
    ( handleLogin
    , handleRegister
) where

import Happstack.Server
import Web.Backend.Auth
import Web.Frontend
import Control.Monad (msum)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!), docTypeHtml, toHtml)

loginPageStyles :: H.Html
loginPageStyles = H.style $ do
    "body {"
    "    font-family: Arial, sans-serif;"
    "    background-color: #f4f4f9;"
    "    display: flex;"
    "    justify-content: center;"
    "    align-items: center;"
    "    height: 100vh;"
    "    margin: 0;"
    "}"
    ".login-container {"
    "    background-color: #fff;"
    "    padding: 20px;"
    "    border-radius: 8px;"
    "    box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);"
    "    width: 300px;"
    "}"
    ".login-container h1 {"
    "    text-align: center;"
    "    color: #333;"
    "}"
    ".login-container form {"
    "    display: flex;"
    "    flex-direction: column;"
    "}"
    ".login-container label {"
    "    margin-bottom: 5px;"
    "    color: #666;"
    "}"
    ".login-container input[type='text'], .login-container input[type='password'] {"
    "    padding: 10px;"
    "    margin-bottom: 15px;"
    "    border: 1px solid #ccc;"
    "    border-radius: 4px;"
    "}"
    ".login-container input[type='submit'] {"
    "    padding: 10px;"
    "    background-color: #007BFF;"
    "    color: #fff;"
    "    border: none;"
    "    border-radius: 4px;"
    "    cursor: pointer;"
    "}"
    ".login-container input[type='submit']:hover {"
    "    background-color: #0056b3;"
    "}"

-- 登录页面的HTML
loginPage :: Maybe T.Text -> H.Html
loginPage txt = docTypeHtml $ do
    H.head $ do
        H.title "登录页面"
        loginPageStyles
        addIcon
    H.body $ do
        H.div ! A.class_ "login-container" $ do
            H.h1 "请登录"
            H.form ! A.action "/login" ! A.method "post" $ do
                H.label ! A.for "username" $ "用户名: "
                H.input ! A.type_ "text" ! A.id "username" ! A.name "username"
                H.br
                H.label ! A.for "password" $ "密码: "
                H.input ! A.type_ "password" ! A.id "password" ! A.name "password"
                H.br
                H.input ! A.type_ "submit" ! A.value "登录"
                H.br
                H.input ! A.type_ "button" ! A.value "注册" ! A.onclick "window.location.href='/register'"
                H.br
                case txt of
                    Nothing -> H.br
                    Just x -> H.label ! A.for "error" $ toHtml x

registerPage :: Maybe T.Text -> H.Html
registerPage txt = docTypeHtml $ do
    H.head $ do
        H.title "登录"
        loginPageStyles
        addIcon
    H.body $ do
        H.div ! A.class_ "login-container" $ do
            H.h1 "请登录"
            H.form ! A.action "/register" ! A.method "post" $ do
                H.label ! A.for "username" $ "用户名: "
                H.input ! A.type_ "text" ! A.id "username" ! A.name "username"
                H.br
                H.label ! A.for "password" $ "密码: "
                H.input ! A.type_ "password" ! A.id "password" ! A.name "password"
                H.br
                H.label ! A.for "invitecode" $ "邀请码: "
                H.input ! A.type_ "text" ! A.id "invitecode" ! A.name "invitecode"
                H.br
                H.input ! A.type_ "submit" ! A.value "注册"
                H.br
                H.input ! A.type_ "button" ! A.value "登录" ! A.onclick "window.location.href='/login'"
                case txt of
                    Nothing -> H.br
                    Just x -> H.label ! A.for "error" $ toHtml x

handleLogin :: ServerPart Response
handleLogin = msum 
    [ verifyUserLevel 
        [ method GET >> (ok $ toResponse $ loginPage Nothing)
        , method GET >> (seeOther ("/") (toResponse ()))
        ]
    , method POST >> do
        userRes <- login
        case userRes of
            Left err -> ok $ toResponse $ loginPage (Just $ T.pack err)
            Right user -> seeOther ("/") (toResponse ())
    ]

handleRegister :: ServerPart Response
handleRegister = msum
    [ verifyUserLevel 
        [ method GET >> (ok $ toResponse $ registerPage Nothing)
        , method GET >> (seeOther "/" (toResponse ()))
        ]
    , method POST >> do
        userRes <- register
        case userRes of
            Left err -> ok $ toResponse $ registerPage (Just $ T.pack err)
            Right user -> seeOther ("/") (toResponse ())
    ]