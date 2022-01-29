{-# LANGUAGE OverloadedStrings #-}

module Putio.Transfer
  ( list,
    get,
    add,
    retry,
    module Data,
  )
where

import Data.Aeson (KeyValue ((.=)), Value, object)
import Data.Text (Text)
import Putio.Client (transferClient)
import Putio.Core
import Putio.Data.Transfer as Data
import Putio.Data.Wrap
import Servant.API
import Servant.Client

_list :: Maybe Token -> ClientM (Wrap [Transfer])
_get :: Integer -> Maybe Token -> ClientM (Wrap Transfer)
_add :: Value -> Maybe Token -> ClientM (Wrap Transfer)
_retry :: Maybe Integer -> Maybe Token -> ClientM (Wrap Transfer)
_list :<|> _get :<|> _add :<|> _retry = transferClient

list :: PutioM [Transfer]
list = unwrap <$> inPutioM _list

get :: Integer -> PutioM Transfer
get transId = unwrap <$> inPutioM (_get transId)

add :: Text -> Maybe Integer -> Maybe Text -> PutioM Transfer
add url parentId callback = unwrap <$> inPutioM (_add body)
  where
    body = object ["url" .= url, "save_parent_id" .= parentId, "callback_url" .= callback]

retry :: Integer -> PutioM Transfer
retry transId = unwrap <$> inPutioM (_retry $ Just transId)