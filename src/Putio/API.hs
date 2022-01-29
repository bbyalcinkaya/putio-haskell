{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Putio.API (Api) where

import Data.Proxy
import qualified Putio.API.Account as Account
import qualified Putio.API.File as File
import qualified Putio.API.Transfer as Transfer
import Putio.Core
import Servant.API
import Servant.Client

type Api = "v2" :> Api'

type Api' =
  "account" :> Account.Api
    :<|> "files" :> File.Api
    :<|> "transfers" :> Transfer.Api
