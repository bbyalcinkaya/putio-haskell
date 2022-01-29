{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Putio.Data.Transfer where

import Data.Aeson (FromJSON, ToJSON, parseJSON, genericParseJSON, defaultOptions, fieldLabelModifier,toJSON, genericToJSON) 
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Data.Text (Text)

data Transfer = Transfer 
    { availability :: Integer
    , created_at :: LocalTime 
    , current_ratio :: Double
    , downloaded :: Integer 
    , uploaded :: Integer 
    , down_speed :: Integer 
    , up_speed :: Integer
    , error_message :: Maybe Text
    , estimated_time :: Maybe Integer
    , file_id :: Integer 
    , finished_at :: LocalTime 
    , id :: Integer 
    , is_private :: Bool
    , name :: Text
    , peers :: Maybe Integer
    , percent_done :: Integer
    , save_parent_id :: Integer
    , seconds_seeding :: Integer
    , size :: Integer
    , source :: Text
    , status :: Status
    , subscription_id :: Maybe Integer
    , tracker_message :: Maybe Text
    , type' :: Type
    }
  deriving (Eq, Show, Generic)

instance FromJSON Transfer where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = labelMod }
    where 
      labelMod "type'" = "type" 
      labelMod a = a

instance ToJSON Transfer where 
  toJSON = genericToJSON defaultOptions
    where 
      labelMod "type'" = "type" 
      labelMod a = a

data Status = IN_QUEUE | WAITING | DOWNLOADING | COMPLETING | SEEDING | COMPLETED | ERROR deriving (Eq, Show, Generic)

instance FromJSON Status 

instance ToJSON Status

data Type = TORRENT | URL | PLAYLIST deriving (Eq, Show, Generic)

instance FromJSON Type

instance ToJSON Type 