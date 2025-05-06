module Web.Backend.Server.Login
    ( handleLogin
    , handleRegister
) where

import Happstack.Server
import qualified Web.Backend.Auth as A
import Web.Frontend
import Control.Monad (msum)
import qualified Data.Text as T

handleLogin :: ServerPart Response
handleLogin = msum 
    [ A.verifyUserLevel 
        [ method GET >> (ok $ toResponse $ loginPage Nothing)
        , method GET >> (seeOther ("/") (toResponse ()))
        ]
    , method POST >> do
        userRes <- A.login
        case userRes of
            Left err -> ok $ toResponse $ loginPage (Just $ T.pack err)
            Right user -> seeOther ("/") (toResponse ())
    ]

handleRegister :: ServerPart Response
handleRegister = msum
    [ A.verifyUserLevel 
        [ method GET >> (ok $ toResponse $ registerPage Nothing)
        , method GET >> (seeOther "/" (toResponse ()))
        ]
    , method POST >> do
        userRes <- A.register
        case userRes of
            Left err -> ok $ toResponse $ registerPage (Just $ T.pack err)
            Right user -> seeOther ("/") (toResponse ())
    ]