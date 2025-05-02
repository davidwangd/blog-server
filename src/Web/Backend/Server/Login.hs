module Web.Backend.Server.Login
    ( handleLogin
    , handleRegister
) where

import Happstack.Server
import qualified Web.Backend.Auth as A
import Web.Frontend

handleLogin :: ServerPart Response
handleLogin = msum 
    [ method GET >> A.verifyUserLevel 
        [ ok $ toResponsen $ loginPage Nothing
        , seeOther ("/") (toResponse ())
        ]
    , method POST >> do
        userRes <- A.login
        case userRes of
            Left err -> ok $ toResponse $ loginPage (Just err)
            Right user -> seeOther ("/") (toResponse ())
    ]

handleRegister :: ServerPart Response
handleRegister = msum
    [ method GET >> A.verifyUserLevel
        [ ok $ toResponse $ registerPage Nothing
        , seeOther ("/") (toResponse ())
        ]
    , method POST >> do
        userRes <- A.register
        case userRes of
            Left err -> ok $ toResponse $ registerPage (Just err)
            Right user -> seeOther ("/") (toResponse ())
    ]