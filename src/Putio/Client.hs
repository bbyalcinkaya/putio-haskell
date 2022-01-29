{-# LANGUAGE TypeApplications #-}

module Putio.Client where

import Data.Proxy
import qualified Putio.API as PutioApi
import qualified Putio.API.Account as Account
import qualified Putio.API.File as File
import qualified Putio.API.Transfer as Transfer
import Putio.Core (ClientOf)
import Servant.API
import Servant.Client (client)

accountClient :: ClientOf Account.Api
fileClient :: ClientOf File.Api
transferClient :: ClientOf Transfer.Api
accountClient :<|> fileClient :<|> transferClient = client (Proxy @PutioApi.Api)