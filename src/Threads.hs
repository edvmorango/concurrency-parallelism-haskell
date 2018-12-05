module Threads where

import Control.Concurrent
import Control.Monad
import System.IO
import Text.Printf

forkEG :: IO ()
forkEG = do
  _ <- hSetBuffering stdout NoBuffering
  _ <- forkIO $ replicateM_ 10000 (putChar 'A')
  replicateM_ 10000 (putChar 'B')

reminderMain :: IO ()
reminderMain =
  forever $ do
    t <- getLine
    forkIO (setReminder t)

setReminder :: String -> IO ()
setReminder s = do
  let t = (read s :: Int)
  _ <- printf "Will remind in %d seconds\n" t
  _ <- threadDelay (10 ^ 6 * t)
  printf "\n%d seconds has passed" t

--  forkIO threads are like Java daemon threads
--  when `return ()` happens they are just thrown away
exit :: IO ()
exit = loop
  where
    loop = do
      s <- getLine
      if s == "exit"
        then return ()
        else do
          id <- forkIO $ putStrLn "Don't exited"
          loop
