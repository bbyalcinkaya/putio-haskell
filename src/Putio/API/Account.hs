{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Putio.API.Account where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor ((<&>))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Putio.Core
import Putio.Data.Account
import Putio.Data.Wrap
import Servant.API
import Servant.Client

type Api = "info" :> AuthHeader
                  :> Get '[JSON] (Wrap AccountInfo)
      :<|> "settings" :> AuthHeader
                      :> Get '[JSON] (Wrap AccountSettings)

