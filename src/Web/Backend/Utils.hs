module Web.Backend.Utils 
    ( formatData
    , formatData'
) where

import Data.Time.Clock
import Data.Time.Format

import qualified Data.Text as T

formatDate :: UTCTime -> T.Text
formatDate = T.pack . formatTime defaultTimeLocale "%Y年%m月%d日 %H:%M"

formatData' :: T.Text -> T.Text
formatData' = formatData . read . T.unpack