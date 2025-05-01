{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Backend.Data 
    ( User(..)
    , UserInfo(..)
    , Article(..)
    , Resource(..)
    , def
    , tableName
    , getId
    , fields
    ) where

import Data.Text (Text)
import GHC.Generics
import Data.Data
import Data.Default (Default (def))
import Web.Backend.MakeData

data User = User { userId :: Int
                 , name :: Text
                 , level :: Int 
                 } 
    deriving (Show, Eq, Generic, Data, Typeable)

defaultUser :: User
defaultUser = User (-1) "" (-1)

instance Default User where
    def = defaultUser

data UserInfo = UserInfo { userInfoId :: Int
                         , username :: Text
                         , password :: Text
                         }
    deriving (Show, Eq, Generic, Data, Typeable)

instance Default UserInfo where
    def = UserInfo (-1) "" ""

data Article = Article { articleId :: Int
                       , author :: Int
                       , createdAt :: Text
                       , updatedAt :: Text
                       , title :: Text
                       , content :: Text
                       , hasURL :: Bool
                       }
    deriving (Show, Eq, Generic, Data, Typeable)

instance Default Article where
    def = Article { articleId = -1
                  , author = -1 
                  , createdAt = "2000-01-01 00:00"
                  , updatedAt = "2000-01-01 00:00"
                  , title = ""
                  , content = ""
                  , hasURL = False
                  }

data Resource = Resource { resourceId :: Int
                         , uploader :: Int
                         , url :: Text
                         , localPath :: Text
                         , accessiblity :: Int
                         }
    deriving (Show, Eq, Generic, Data, Typeable)

instance Default Resource where
    def = Resource { resourceId = -1
                   , uploader = -1
                   , url = "/404"
                   , localPath = "/"
                   , accessiblity = -1
                   }

makeDBInstance ''User
makeDBInstance ''UserInfo
makeDBInstance ''Article
makeDBInstance ''Resource
