module Web.Backend.Server
    ( server
    )
where

import Web.Backend.Server.Login
import Web.Frontend.Homepage
import Happstack.Server
import Web.Backend.Auth (getUser)
import Web.Frontend.Template
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T

import Web.Backend.Server.Articles
import Web.Backend.Server.ViewArticle
import Web.Backend.Server.Editor
import Web.Backend.Server.Login

-- homepage :: ServerPart Response
-- homepage = do
--     user <- getUser
--     ok $ toResponse $ homepagePage user

homepagePage :: Maybe User -> H.Html
homepagePage user = addHeadTitle title body
    where title = case user of
            Nothing -> T.pack "Hello"
            Just user -> T.pack $ "Hello, " ++ (T.unpack $ username user)
          body = H.div $ do 
            case user of
                Nothing -> H.div $ do
                    H.p "Please Login"
                    H.a ! A.href "/login" $ "Login"
                Just user -> H.p $ toHtml $ ("Hello " ++ (T.unpack $ username user))
            H.a ! A.href "/articles" $ "Articles"

homepage :: ServerPart Response
homepage = do
    user <- getUser
    ok $ toResponse $ homepagePage user

server :: ServerPart Response
server = msum 
    [ dir "login" handleLogin
    , dir "register" handleRegister
    , dir "editor" handleEditor
    , dir "articles" handleArticles
    , dir "view_article" handleViewArticle
    , homepage
    ]
