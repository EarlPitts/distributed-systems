import Control.Concurrent.STM
import Control.Monad

swap :: TVar Int -> TVar Int -> STM ()
swap a b = do
  a' <- readTVar a
  b' <- readTVar b
  writeTVar a b'
  writeTVar b a'

bubbleSort :: [TVar Int] -> STM ()
bubbleSort [] = return ()
bubbleSort [_] = return ()
bubbleSort (v1:v2:vs) = do
  n <- readTVar v1
  m <- readTVar v2
  when (n > m) (swap v1 v2)
  bubbleSort (v2:vs)

main :: IO ()
main = do
-- Swaps two numbers atomically
  a <- newTVarIO 3
  b <- newTVarIO 2
  atomically (swap a b)
  a' <- readTVarIO a
  b' <- readTVarIO b
  print a'
  print b'

  -- Sequential bubble sort with mutable variables 
  ns <- mapM newTVarIO [100,99..0]
  replicateM_ (length ns) $ atomically $ bubbleSort ns
  sorted <- mapM readTVarIO ns
  print sorted
