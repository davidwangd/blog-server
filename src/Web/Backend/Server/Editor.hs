{-# LANGUAGE OverloadedStrings #-}

module Web.Backend.Server.Editor where
    (

    )

import Web.Backend.Auth
import Web.Backend.Data
import Web.Frontend.Editor
import Web.Frontend.Template
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

editorGET :: String -> ServerPart Response
editorGET name = method GET >> verifyUserLevel
    [ seeOther ("/login") $ toResponse "Please login first"
    , seeOther ("/login") $ toResponse "You don't have permission to access this page"
    , ok $ toResponse $ addHeadTitle "编辑" $ editorPage name
    ]

editorPOST :: String -> ServerPart Response
editorPOST _ = method POST >> verifyUserLevel 
    [ ok $ toResponse $ "Noth authed"
    , ok $ toResponse $ "Noth authed"
    , do
        contents <- lookText "contents"
        title <- lookText "title"
        
        
    ]