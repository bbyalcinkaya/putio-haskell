{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Putio.Core
  ( PutioM,
    inPutioM,
    runPutioM,
    runPutio,
    Token,
    AuthHeader,
    ClientOf
  )
where

import Control.Monad.Reader
import Data.Text
import Network.HTTP.Client (Manager)
import Servant.Client
import Servant.API (Header)

-- | API auth token
type Token = Text

type PutioM a = ReaderT Token ClientM a

type AuthHeader = Header "Authorization" Token

runPutioM :: PutioM a -> Token -> ClientM a
runPutioM = runReaderT

inPutioM :: (Maybe Token -> ClientM a) -> PutioM a
inPutioM clientAction = do 
  authToken <- ask
  liftIO $ print authToken
  lift . clientAction $ Just authToken

apiBaseUrl :: String
apiBaseUrl = "api.put.io"

runPutio :: PutioM a -> Manager -> Token -> IO (Either ClientError a)
runPutio putioM manager token = runClientM clientM clientEnv
  where
    clientM = runPutioM putioM $ "Bearer " <> token
    clientEnv = mkClientEnv manager (BaseUrl Https apiBaseUrl 443 "")

type ClientOf a = Client ClientM a
