{-# LANGUAGE OverloadedStrings #-}

module Web.Frontend.Homepage
    ( homepagePage
    ) where


import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Backend.Data
import Web.Frontend.Template
import Text.Blaze.Html5 ((!), toHtml)
import qualified Data.Text as T

homepagePage :: Maybe User -> H.Html
homepagePage user = addHeadTitle title body
    where title = case user of
            Nothing -> T.pack "Hello"
            Just user -> T.pack $ "Hello, " ++ (T.unpack $ username user)
          body = case user of
            Nothing -> H.div $ do
                H.p "Please Login"
                H.a ! A.href "/login" $ "Login"
            Just user -> H.p $ toHtml $ ("Hello " ++ (T.unpack $ username user))
