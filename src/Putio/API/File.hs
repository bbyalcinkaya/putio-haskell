{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Putio.API.File where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON, Value, object)
import Data.Proxy
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Putio.Core
import Putio.Data.File (File, FileType)
import Putio.Data.Wrap (Wrap)
import Servant.API

type Api =
  "list"
    :> QueryParam "parent_id" Integer
    :> QueryParam "per_page" Integer
    :> QueryParam "sort_by" SortBy
    :> QueryParam "file_type" FileType
    :> QueryParam "stream_url" Text
    :> QueryParam "stream_url_parent" Text
    :> QueryParam "mp4_stream_url" Text
    :> QueryParam "mp4_stream_url_parent" Text
    :> QueryParam "hidden" Bool
    :> QueryParam "mp4_status" Bool
    :> AuthHeader
    :> Get '[JSON] ListRes
    :<|> "list" :> "continue"
      :> ReqBody '[JSON] Value
      :> AuthHeader
      :> Post '[JSON] ListContRes
    :<|> "search"
      :> QueryParam' '[Required] "query" Text
      :> QueryParam "per_page" Integer
      :> AuthHeader
      :> Get '[JSON] SearchRes
    :<|> "search" :> "continue"
      :> ReqBody '[JSON] Value
      :> AuthHeader
      :> Post '[JSON] SearchRes
    :<|> "create-folder"
      :> ReqBody '[JSON] Value
      :> AuthHeader
      :> Post '[JSON] (Wrap File)
    :<|> "rename"
      :> ReqBody '[JSON] Value
      :> AuthHeader
      :> Post '[JSON] ()
    :<|> "move"
      :> ReqBody '[JSON] Value
      :> AuthHeader
      :> Post '[JSON] ()
    :<|> Capture "id" Integer
      :> "mp4"
      :> AuthHeader
      :> Post '[JSON] ()
    :<|> Capture "id" Integer
      :> "url"
      :> AuthHeader
      :> Get '[JSON] UrlRes
    :<|> "delete"
      :> ReqBody '[JSON] Value
      :> AuthHeader
      :> Post '[JSON] Value

data SortBy
  = NAME_ASC
  | NAME_DESC
  | SIZE_ASC
  | SIZE_DESC
  | DATE_ASC
  | DATE_DESC
  | MODIFIED_ASC
  | MODIFIED_DESC
  deriving (Eq, Show, Generic)

instance FromJSON SortBy

instance ToJSON SortBy

instance ToHttpApiData SortBy where 
  toQueryParam = pack . show

data ListRes = ListRes
  { files :: [File],
    parent :: File,
    total :: Integer,
    cursor :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON ListRes

instance ToJSON ListRes

data ListContRes = ListContRes
  { files :: [File],
    cursor :: Text
  }
  deriving (Show, Generic)

instance FromJSON ListContRes

instance ToJSON ListContRes

data SearchRes = SearchRes
  { files :: [File],
    total :: Integer,
    cursor :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON SearchRes

instance ToJSON SearchRes

newtype UrlRes = UrlRes
  { url :: Text
  }
  deriving (Show, Generic)

instance FromJSON UrlRes

instance ToJSON UrlRes

