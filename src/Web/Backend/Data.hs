{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Backend.Data 
    ( User(..)
    , Article(..)
    , Resource(..)
    , def
    , tableName
    , getId
    , fields
    , createTableStmt
    , dataFields
    , insert
    , getAll
    , remove
    , queryById
    ) where

import Data.Text (Text)
import GHC.Generics
import Data.Data
import Data.Default (Default (def))
import Web.Backend.MakeData

data User = User { userId :: Int
                 , username :: Text
                 , password :: Text
                 , name :: Text
                 , level :: Int 
                 } 
    deriving (Show, Eq, Generic, Data, Typeable)

instance Default User where
    def = User { userId = -1
               , username = "none"
               , password = ""
               , name = ""
               , level = -1
               }

data Article = Article { articleId :: Int
                       , author :: Int
                       , createdAt :: Text
                       , updatedAt :: Text
                       , title :: Text
                       , content :: Text
                       , hasUrl :: Bool
                       , articleAccessiblity :: Int
                       }
    deriving (Show, Eq, Generic, Data, Typeable)

instance Default Article where
    def = Article { articleId = -1
                  , author = -1 
                  , createdAt = "2000-01-01 00:00"
                  , updatedAt = "2000-01-01 00:00"
                  , title = ""
                  , content = ""
                  , hasUrl = False
                  , articleAccessiblity = 0
                  }

data Resource = Resource { resourceId :: Int
                         , uploader :: Int
                         , url :: Text
                         , localPath :: Text
                         , resourceAccessiblity :: Int
                         }
    deriving (Show, Eq, Generic, Data, Typeable)

instance Default Resource where
    def = Resource { resourceId = -1
                   , uploader = -1
                   , url = "/404"
                   , localPath = "/"
                   , resourceAccessiblity = -1
                   }

makeDBInstance ''User
makeDBInstance ''Article
makeDBInstance ''Resource
