import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

showTList :: [TVar Int] -> IO ()
showTList = print <=< mapM readTVarIO

swap :: TVar Int -> TVar Int -> STM ()
swap a b = do
  a' <- readTVar a
  b' <- readTVar b
  writeTVar a b'
  writeTVar b a'

bubbleSort :: [TVar Int] -> STM ()
bubbleSort [] = return ()
bubbleSort [_] = return ()
bubbleSort (v1 : v2 : vs) = do
  n <- readTVar v1
  m <- readTVar v2
  when (n > m) (swap v1 v2)
  bubbleSort (v2 : vs)

-- Atomic swap
swapAtIndex :: [TVar Int] -> Int -> STM ()
swapAtIndex tvars i = do
  t1 <- readTVar $ tvars !! i
  t2 <- readTVar $ tvars !! (i + 1)
  when (t1 > t2) (swap (tvars !! i) (tvars !! (i + 1)))

-- Non-atomic, deadlocks are possible
swapAtIndex' :: [MVar Int] -> Int -> IO ()
swapAtIndex' ms i = do
  n <- takeMVar nInd
  m <- takeMVar mInd
  when (n > m) $ do
    putMVar nInd m
    putMVar mInd n
  where
    nInd = ms !! i
    mInd = ms !! (i + 1)

-- This will usually block, because swapAtIndex' is not an atomic operation
swapConcTest :: IO ()
swapConcTest = do
  ns <- mapM newMVar [10, 9 .. 0]
  ts <- mapM (async . replicateM 10 . swapAtIndex' ns) [0 .. 9]
  mapM_ wait ts
  s <- mapM readMVar ns
  print s

-- Swaps two numbers atomically
swapTest :: IO ()
swapTest = do
  a <- newTVarIO 3
  b <- newTVarIO 2
  atomically (swap a b)
  a' <- readTVarIO a
  b' <- readTVarIO b
  print a'
  print b'

-- Sequential bubble sort with mutable variables
seqBubbleTest :: IO ()
seqBubbleTest = do
  ns <- mapM newTVarIO [10, 9 .. 0]
  threads <- replicateM (length ns) $ async $ atomically $ bubbleSort ns
  mapM_ wait threads
  sorted <- mapM readTVarIO ns
  print sorted

-- Concurrent bubble sort with atomic swaps
concBubbleTest :: IO ()
concBubbleTest = do
  ns <- mapM newTVarIO [10, 9 .. 0]
  -- Start a new thread for each index, swapping the numbers if needed
  threads <- mapM (async . forever . atomically . swapAtIndex ns) [0..9]
  t <- async $ forever $ showTList ns -- Start a new thread for tracing the list
  threadDelay 2000000 -- Wait two seconds
  mapM_ cancel threads
  cancel t
