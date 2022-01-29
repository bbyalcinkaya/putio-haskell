{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Putio.Data.File where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData (toQueryParam))
import Data.Text (pack)

data FileType = FOLDER | FILE | AUDIO | VIDEO | IMAGE | ARCHIVE | PDF | TEXT | SWF deriving (Eq, Show, Generic)

instance FromJSON FileType

instance ToJSON FileType

instance ToHttpApiData FileType where 
  toQueryParam = pack . show

data File = File
  { content_type :: String,
    created_at :: LocalTime,
    id :: Integer,
    is_mp4_available :: Bool,
    is_shared :: Bool,
    name :: String,
    parent_id :: Maybe Integer,
    screenshot :: Maybe String,
    size :: Integer,
    file_type :: FileType,
    extension :: Maybe String,
    need_convert :: Maybe Bool,
    mp4_size :: Maybe Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON File

instance ToJSON File


