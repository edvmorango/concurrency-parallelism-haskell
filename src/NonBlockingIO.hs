{-# LANGUAGE OverloadedStrings #-}

module NonBlockingIO where

import Control.Concurrent
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Client
import Network.HTTP.Client.TLS

getUrl :: String -> IO B.ByteString
getUrl url = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest url
  res <- httpLbs req manager
  return $ (B.take 100 . responseBody) res
