{-# LANGUAGE OverloadedStrings #-}

module NonBlockingIO where

import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Lazy as B
import Data.Typeable
import Debug.Trace
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.TimeIt
import Text.Printf

getUrl :: String -> IO B.ByteString
getUrl url = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest url
  res <- httpLbs req manager
  return $ (B.take 100 . responseBody) res

data Async a =
  Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  mv <- newEmptyMVar
  _ <- (forkIO $ (try action) >>= (putMVar mv)) >>= traceThread
  return $ Async mv

traceThread :: ThreadId -> IO ThreadId
traceThread a = trace (show a) (return a)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async var) = readMVar var

-- Must rewrap to avoid deadlocks and provide the value to other consumers
wait :: Async a -> IO a
wait a = do
  v <- waitCatch a
  case v of
    Left e -> throwIO e
    Right a -> return a

mainLowLevel :: IO ()
mainLowLevel = do
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

mainAsync :: IO ()
mainAsync = do
  a1 <- async $ getUrl "https://www.gitlab.com"
  a2 <- async $ getUrl "https://www.github.com"
  m1 <- wait a1
  m2 <- wait a2
  putStrLn "Finished"

mainAsyncFail :: IO ()
mainAsyncFail = do
  a1 <- async $ getUrl "https://www.gitlab.comm"
  a2 <- async $ getUrl "https://www.github.com"
  m1 <- wait a1
  m2 <- wait a2
  putStrLn "Finished"

urls :: [String]
urls =
  ["https://www.gitlab.com", "https://www.github.com", "https://bitbucket.com"]

urlsTime :: String -> IO ()
urlsTime url = do
  (time) <- timeIt $ getUrl url
  putStrLn $ "Page: " ++ url

mainUrls = do
  as <- mapM (async . urlsTime) urls
  mapM_ wait as
