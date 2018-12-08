module MVars where

import Control.Concurrent
import qualified Data.Map as M

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

-- Resource sharing
type Name = String

type PhoneNumber = String

type PhoneBook = M.Map Name PhoneNumber

newtype PhoneBookState =
  PhoneBookState (MVar PhoneBook)

newPhoneBook :: IO PhoneBookState
newPhoneBook = do
  m <- newMVar M.empty
  return $ PhoneBookState m

-- $! evaluates before unlock to avoid leaks caused by lazy evaluation
insertContact :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insertContact (PhoneBookState s) name phone = do
  phoneBook <- takeMVar s
  putMVar s $! M.insert name phone phoneBook

peekPhone :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
peekPhone (PhoneBookState s) name = do
  phoneBook <- takeMVar s
  _ <- putMVar s phoneBook
  return $ M.lookup name phoneBook

phoneBookMain :: IO ()
phoneBookMain = do
  s <- newPhoneBook
  sequence_ [insertContact s ("name" ++ show n) (show n) | n <- [1 .. 10000]]
  peekPhone s "name999" >>= print
  peekPhone s "some" >>= print

printPhone :: Int -> String -> Maybe String -> IO ()
printPhone d s Nothing = do
  _ <- threadDelay (10 ^ 6 * d)
  putStrLn $ s ++ ": Nothing"
printPhone d s (Just a) = do
  _ <- threadDelay (10 ^ 6 * d)
  putStrLn $ s ++ ": " ++ a

peekInThread :: Int -> String -> PhoneBookState -> IO ()
peekInThread delay threadName s =
  sequence_
    [ (peekPhone s ("name" ++ show n) >>= (printPhone delay threadName))
    | n <- [1 .. 1000]
    ]

phoneBookConcurrentMain :: IO ()
phoneBookConcurrentMain = do
  s <- newPhoneBook
  _ <- sequence_ [insertContact s ("name" ++ show n) (show n) | n <- [1 .. 100]]
  _ <- forkIO $ peekInThread 1 "Thread 1" s
  _ <- forkIO $ peekInThread 2 "Thread 2" s
  _ <- peekPhone s "name1"
  _ <- threadDelay (10 ^ 6 * 10000)
  putStrLn "Finished"
