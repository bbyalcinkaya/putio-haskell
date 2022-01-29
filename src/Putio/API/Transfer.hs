{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Putio.API.Transfer where

import Data.Aeson (Value)
import Data.Text (Text)
import Putio.Core (AuthHeader)
import Putio.Data.Transfer (Transfer)
import Putio.Data.Wrap
import Servant.API

type Api =
  "list"
    :> AuthHeader
    :> Get '[JSON] (Wrap [Transfer])
    :<|> Capture "id" Integer
      :> AuthHeader
      :> Get '[JSON] (Wrap Transfer)
    :<|> "add"
      :> ReqBody '[JSON] Value
      :> AuthHeader
      :> Post '[JSON] (Wrap Transfer)
    :<|> "retry"
      :> QueryParam "id" Integer
      :> AuthHeader
      :> Post '[JSON] (Wrap Transfer)
