module MVars where

-- newEmptyMvar :: IO (MVar a) 
-- newMVar :: a -> IO (MVar a)
-- takeMVar :: MVar a -> IO a
-- putMVar a -> a -> IO ()
mvarEG :: IO ()
mvarEG = do
  m <- newEmptyMvar
  _ <- forkIO $ putMVar m "some value"
  r <- takeMVar m
  print r
