{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Putio.File
  ( list,
    listCont,
    search,
    searchCont,
    createFolder,
    rename,
    move,
    convert,
    url,
    ListArgs(..),
    withArgs,
    ListRes (..),
    ListContRes (..),
    SearchRes (..),
    module Data,
  )
where

import Data.Aeson (KeyValue ((.=)), Value, object)
import Data.Text (Text)
import Putio.API.File (ListContRes, ListRes, SearchRes, SortBy, UrlRes)
import Putio.Client (fileClient)
import Putio.Core
import Putio.Data.File (File (parent_id))
import Putio.Data.File as Data
import Putio.Data.Wrap
import Servant.API
import Servant.Client

_listCont :: Value -> Maybe Token -> ClientM ListContRes
_search :: Text -> Maybe Integer -> Maybe Token -> ClientM SearchRes
_searchCont :: Value -> Maybe Token -> ClientM SearchRes
_createFolder :: Value -> Maybe Token -> ClientM (Wrap File)
_rename :: Value -> Maybe Token -> ClientM ()
_move :: Value -> Maybe Token -> ClientM ()
_convertMp4 :: Integer -> Maybe Token -> ClientM ()
_url :: Integer -> Maybe Token -> ClientM UrlRes
_list ::
  Maybe Integer ->
  Maybe Integer ->
  Maybe SortBy ->
  Maybe FileType ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Token ->
  ClientM ListRes

_list :<|> _listCont :<|> _search :<|> _searchCont :<|> _createFolder :<|> _rename :<|> _move :<|> _convertMp4 :<|> _url = fileClient

data ListArgs = Args
  { parent_id :: Maybe Integer,
    per_page :: Maybe Integer,
    sort_by :: Maybe SortBy,
    file_type :: Maybe FileType,
    stream_url :: Maybe Text,
    stream_url_parent :: Maybe Text,
    mp4_stream_url :: Maybe Text,
    mp4_stream_url_parent :: Maybe Text,
    hidden :: Maybe Bool,
    mp4_status :: Maybe Bool
  }

withArgs :: ListArgs
withArgs = Args Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

list :: ListArgs -> PutioM ListRes
list Args {..} = inPutioM $ _list parent_id per_page sort_by file_type stream_url stream_url_parent mp4_stream_url 
                                  mp4_stream_url_parent hidden mp4_status

listCont :: Text -> Maybe Integer -> PutioM ListContRes
listCont cursor perPage = inPutioM . _listCont $ object ["cursor" .= cursor, "per_page" .= perPage]

search :: Text -> Maybe Integer -> PutioM SearchRes
search query = inPutioM . _search query

searchCont :: Text -> Maybe Integer -> PutioM SearchRes
searchCont cursor perPage = inPutioM . _searchCont $ object ["cursor" .= cursor, "per_page" .= perPage]

createFolder :: Integer -> Text -> PutioM File
createFolder parentId name = unwrap <$> (inPutioM . _createFolder $ object ["parent_id" .= parentId, "name" .= name])

rename :: Integer -> Text -> PutioM ()
rename fileId newName = inPutioM . _rename $ object ["file_id" .= fileId, "name" .= newName]

move :: [Integer] -> Integer -> PutioM ()
move fileIds parentId = inPutioM . _move $ object ["file_ids" .= fileIds, "parent_id" .= parentId]

convert :: Integer -> PutioM ()
convert fileId = inPutioM $ _convertMp4 fileId

url :: Integer -> PutioM UrlRes
url fileId = inPutioM $ _url fileId
