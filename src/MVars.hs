module MVars where

import Control.Concurrent

-- newEmptyMVar :: IO (MVar a)
-- newMVar :: a -> IO (MVar a)
-- takeMVar :: MVar a -> a
-- putMVar :: MVar a -> a -> IO ()
mvarEG = do
  m <- newEmptyMVar
  _ <- forkIO $ putMVar m "some value"
  r <- takeMVar m
  print r

-- `takeMVar` is blocking like Java classic `Future` 
mvarEG1 = do
  m <- newEmptyMVar
  _ <- forkIO $ delay m
  r <- takeMVar m
  print r

delay :: MVar String -> IO ()
delay mv = do
  _ <- threadDelay (10 ^ 6 * 5) -- 5 seconds
  putMVar mv "finished"

---Logger
data Logger =
  Logger (MVar LogCommand)

data LogCommand
  = Message String
  | Stop (MVar ())

-- Creates a reference (MVar) and starts a loop inside another thread
initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  _ <- forkIO (logger l)
  return l

-- A loop who works around a reference (MVar) modified by another thread
logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          _ <- putStrLn msg
          loop
        Stop s -> do
          _ <- putStrLn "Stopping"
          putMVar s ()

-- Updates a reference 
logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  _ <- putMVar m (Stop s)
  takeMVar s

mainLogger :: IO ()
mainLogger = do
  l <- initLogger
  _ <- logMessage l "m1"
  _ <- logMessage l "m2"
  logStop l
