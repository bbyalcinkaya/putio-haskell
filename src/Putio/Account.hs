module Putio.Account
  ( getInfo,
    getSettings,
    module Data,
  )
where

import Putio.Client (accountClient)
import Putio.Core (PutioM, Token, inPutioM)
import Putio.Data.Account as Data
import Putio.Data.Wrap (Wrap (unwrap))
import Servant.API
import Servant.Client (ClientM)

_getInfo :: Maybe Token -> ClientM (Wrap AccountInfo)
_getSettings :: Maybe Token -> ClientM (Wrap AccountSettings)
_getInfo :<|> _getSettings = accountClient

getInfo :: PutioM AccountInfo
getInfo = unwrap <$> inPutioM _getInfo

getSettings :: PutioM AccountSettings
getSettings = unwrap <$> inPutioM _getSettings
