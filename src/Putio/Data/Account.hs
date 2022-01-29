{-# LANGUAGE DeriveGeneric #-}

module Putio.Data.Account where 

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics (Generic)
import Data.Text (Text)



data AccountInfo = AccountInfo
  { user_id :: Integer,
    username :: String,
    mail :: String,
    monthly_bandwidth_usage :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON AccountInfo

instance FromJSON AccountInfo

data AccountSettings = AccountSettings
  { default_download_folder :: Maybe Integer,
    is_invisible :: Bool,
    subtitle_languages :: [String],
    default_subtitle_language :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance ToJSON AccountSettings

instance FromJSON AccountSettings


