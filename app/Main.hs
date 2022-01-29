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

authToken :: Token
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
  accInfo <- Account.getInfo
  liftIO $ print accInfo 
           >> putStrLn "-------------\n"

  files <- File.list withArgs { per_page = Just 10 }
  liftIO $ print files 
           >> putStrLn "-------------\n"
         
  transfers <- Transfer.list
  liftIO $ print transfers 
           >> putStrLn "-------------\n"
