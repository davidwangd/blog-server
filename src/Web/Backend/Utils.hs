module Web.Backend.Utils 
    ( formatDate
    , formatDate'
) where

import Data.Time.Clock
import Data.Time.Format

import qualified Data.Text as T

formatDate :: UTCTime -> T.Text
formatDate = T.pack . formatTime defaultTimeLocale "%Y年%m月%d日 %H:%M"

formatDate' :: T.Text -> T.Text
formatDate' = formatDate . read . T.unpack