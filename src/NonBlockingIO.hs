{-# LANGUAGE OverloadedStrings #-}

module NonBlockingIO where

import Control.Concurrent
import qualified Data.ByteString.Lazy as B
import Debug.Trace
import Network.HTTP.Client
import Network.HTTP.Client.TLS

getUrl :: String -> IO B.ByteString
getUrl url = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest url
  res <- httpLbs req manager
  return $ (B.take 100 . responseBody) res

main :: IO ()
main = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  _ <-
    forkIO $
    threadDelay (10 ^ 6 * 1) >> (getUrl "https://www.gitlab.org") >>=
    trace "GitLab" (putMVar m1)
  _ <-
    forkIO $ (getUrl "https://www.github.com") >>= trace "GitHub" (putMVar m2)
  r1 <- trace "m1 evaluated" (takeMVar m1)
  r2 <- trace "m2 evaluated" (takeMVar m2)
  putStrLn "Finished"
