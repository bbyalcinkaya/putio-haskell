# putio-haskell

Haskell client library for [Put.io API v2](https://api.put.io/v2/docs) powered by [servant-client](https://hackage.haskell.org/package/servant-client).

## Usage

### Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Putio
import qualified Putio.Account as Account
import qualified Putio.File as File
import Putio.File (ListArgs (..), withArgs)
import qualified Putio.Transfer as Transfer

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Control.Monad.IO.Class (liftIO)

authToken :: Text
authToken = "YOUR_OAUTH_TOKEN"

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  res <- runPutio action manager authToken
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> return ()

action :: PutioM ()
action = do
  -- GET /account/info
  accInfo <- Account.getInfo
  liftIO $ print accInfo 
  -- GET /files/list
  files <- File.list withArgs { per_page = Just 10 }
  liftIO $ print files
  -- GET /transfers/list
  transfers <- Transfer.list
  liftIO $ print transfers
```