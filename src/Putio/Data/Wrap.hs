{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Putio.Data.Wrap where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON, Value, object, parseJSON, toJSON, withObject, (.:))
import Data.Aeson.Key (fromText)
import Data.Proxy
import Data.Text (Text)
import Putio.Data.Account
import Putio.Data.File
import Putio.Data.Transfer

newtype Wrap a = Wrap {unwrap :: a}

class Wrapped a where
  wrapper :: Proxy a -> Text

instance (Wrapped a, ToJSON a) => ToJSON (Wrap a) where
  toJSON (Wrap a) = object [fromText (wrapper (Proxy :: Proxy a)) .= toJSON a]

instance (Wrapped a, FromJSON a) => FromJSON (Wrap a) where
  parseJSON = withObject "" $ \v -> Wrap <$> v .: fromText (wrapper (Proxy :: Proxy a))

instance Wrapped File where
  wrapper _ = "file"

instance Wrapped AccountInfo where
  wrapper _ = "info"

instance Wrapped AccountSettings where
  wrapper _ = "settings"

instance Wrapped [Transfer] where
  wrapper _ = "transfers"

instance Wrapped Transfer where
  wrapper _ = "transfer"
