
-- Haskell Concurrency - Parallel and Concurrent Programming
{-# LANGUAGE BangPatterns #-}

import Control.Concurrent
import Control.Monad
import System.Directory
import System.IO
import Text.Printf

-- | Basic fork and join
forkExample :: IO ()
forkExample = do
    putStrLn "Starting main thread"
    forkIO $ do
        threadDelay 1000000  -- 1 second
        putStrLn "Hello from forked thread!"
    putStrLn "Continuing in main thread"
    threadDelay 2000000  -- Wait for forked thread
    putStrLn "Main thread done"

-- | Multiple threads with synchronization
threadExample :: Int -> IO ()
threadExample n = do
    mv <- newEmptyMVar
    forM_ [1..n] $ \i -> 
        forkIO $ do
            let msg = "Thread " ++ show i
            threadDelay (i * 100000)
            putMVar mv msg
    
    results <- forM [1..n] $ \_ -> takeMVar mv
    mapM_ putStrLn results

-- | Shared variables with MVar
data SharedCounter = SharedCounter 
    { value :: Int
    , mutex :: MVar ()
    }

newCounter :: Int -> IO SharedCounter
newCounter init = do
    m <- newMVar ()
    return $ SharedCounter init m

increment :: SharedCounter -> IO ()
increment counter = do
    withMVar (mutex counter) $ \_ -> do
        let !newVal = value counter + 1
        threadDelay 1000  -- Simulate work
        return ()

-- | TVar for software transactional memory
data Account = Account 
    { balance :: TVar Int
    , accountId :: String
    }

newAccount :: String -> Int -> IO Account
newAccount id initial = do
    bal <- newTVarIO initial
    return $ Account bal id

deposit :: Account -> Int -> STM ()
deposit account amount = do
    current <- readTVar (balance account)
    writeTVar (balance account) (current + amount)

withdraw :: Account -> Int -> STM ()
withdraw account amount = do
    current <- readTVar (balance account)
    if current >= amount
        then writeTVar (balance account) (current - amount)
        else retry

transfer :: Account -> Account -> Int -> STM ()
transfer from to amount = do
    withdraw from amount
    deposit to amount

transferWithTimeout :: Account -> Account -> Int -> Int -> IO Bool
transferWithTimeout from to amount timeout = do
    result <- atomically $ do
        current <- readTVar (balance from)
        if current >= amount
            then do
                withdraw from amount
                deposit to amount
                return $ Right ()
            else return $ Left "Insufficient funds"
    case result of
        Right _ -> return True
        Left _ -> return False

-- | Chan for message passing
data Message = Msg String Int | Quit

messageProcessor :: Chan Message -> IO ()
messageProcessor chan = forever $ do
    msg <- readChan chan
    case msg of
        Msg text count -> putStrLn $ text ++ " " ++ show count
        Quit -> putStrLn "Quitting..."

-- | Async operations
data AsyncResult a = AsyncResult 
    { asyncTask :: Async a
    , result :: a
    }

asyncExample :: IO ()
asyncExample = do
    a1 <- async $ longComputation 1
    a2 <- async $ longComputation 2
    r1 <- wait a1
    r2 <- wait a2
    putStrLn $ "Results: " ++ show (r1 + r2)
    where
        longComputation n = do
            threadDelay (n * 1000000)
            return n

-- | Parallel map
parallelMap :: (a -> b) -> [a] -> IO [b]
parallelMap f xs = do
    let n = length xs
    if n < 1000 
        then return $ map f xs
        else do
            vars <- mapM (async . return . f) xs
            mapM wait vars

-- | Thread pool
data ThreadPool = ThreadPool 
    { workers :: [ThreadId]
    , jobQueue :: Chan (IO ())
    }

createThreadPool :: Int -> IO ThreadPool
createThreadPool size = do
    queue <- newChan
    workers <- forM [1..size] $ \_ -> forkIO $ worker queue
    return $ ThreadPool workers queue
    where
        worker queue = forever $ do
            job <- readChan queue
            job

submitJob :: ThreadPool -> IO () -> IO ()
submitJob pool job = writeChan (jobQueue pool) job

-- | Parallel fold
parFold :: (a -> a -> a) -> a -> [a] -> a
parFold f z [] = z
parFold f z [x] = f z x
parFold f z xs = 
    let (left, right) = splitAt (length xs `div` 2) xs
        leftResult = parFold f z left
        rightResult = parFold f z right
    in f leftResult rightResult

-- | STM retry and orElse
retryExample :: TVar Int -> TVar Int -> STM Int
retryExample var1 var2 = do
    v1 <- readTVar var1
    v2 <- readTVar var2
    if v1 > 0 && v2 > 0
        then return (v1 + v2)
        else retry

orElseExample :: STM Int -> STM Int -> STM Int
orElseExample action1 action2 = orElse action1 action2

-- | Timed operations
withTimeout :: Int -> IO a -> IO (Maybe a)
withTimeout timeout action = do
    result <- newEmptyMVar
    worker <- forkIO $ do
        v <- action
        putMVar result $ Just v
    forkIO $ do
        threadDelay timeout
        killThread worker
        putMVar result Nothing
    takeMVar result

-- | Barrier synchronization
data Barrier = Barrier 
    { count :: TVar Int
    , threshold :: Int
    , condition :: TMVar ()
    }

newBarrier :: Int -> IO Barrier
newBarrier n = do
    c <- newTVarIO 0
    cond <- newEmptyTMVarIO
    return $ Barrier c n cond

waitBarrier :: Barrier -> IO ()
waitBarrier barrier = do
    atomically $ do
        c <- readTVar (count barrier)
        if c + 1 == threshold barrier
            then putTMVar (condition barrier) ()
            else writeTVar (count barrier) (c + 1)
    atomically $ readTMVar (condition barrier)

-- | Lock-free counter
data LockFreeCounter = LockFreeCounter 
    { counterVar :: TVar Int
    }

newLockFreeCounter :: Int -> IO LockFreeCounter
newLockFreeCounter init = do
    v <- newTVarIO init
    return $ LockFreeCounter v

incrementLockFree :: LockFreeCounter -> IO ()
incrementLockFree counter = do
    atomically $ do
        current <- readTVar (counterVar counter)
        writeTVar (counterVar counter) (current + 1)

fetchAndAdd :: TVar Int -> Int -> STM Int
fetchAndAdd var delta = do
    current <- readTVar var
    writeTVar var (current + delta)
    return current

-- | Producer-Consumer
producerConsumer :: Int -> IO ()
producerConsumer bufferSize = do
    chan <- newChan
    done <- newTVarIO False
    
    forkIO $ forM_ [1..100] $ \i -> do
        writeChan chan (Msg i)
    
    forkIO $ forever $ do
        msg <- readChan chan
        case msg of
            Msg n -> putStrLn $ "Received: " ++ show n
    
    threadDelay 2000000

-- | Parallel quicksort
parQuickSort :: (Ord a) => [a] -> [a]
parQuickSort [] = []
parQuickSort [x] = [x]
parQuickSort xs = 
    let pivot = xs !! (length xs `div` 2)
        left = [x | x <- xs, x < pivot]
        right = [x | x <- xs, x > pivot]
        sortedLeft = parQuickSort left
        sortedRight = parQuickSort right
    in sortedLeft ++ [pivot] ++ sortedRight

-- | STM TVar examples
stmExamples :: IO ()
stmExamples = do
    counter <- newTVarIO 0
    
    atomically $ writeTVar counter 42
    val <- atomically $ readTVar counter
    putStrLn $ "Counter value: " ++ show val
    
    atomically $ do
        c <- readTVar counter
        writeTVar counter (c + 1)
    
    newVal <- atomically $ readTVar counter
    putStrLn $ "Incremented: " ++ show newVal

-- | Race condition demonstration
raceCondition :: IO ()
raceCondition = do
    var <- newTVarIO 0
    
    forM_ [1..100] $ \_ -> forkIO $ do
        atomically $ do
            c <- readTVar var
            writeTVar var (c + 1)
    
    threadDelay 100000
    final <- atomically $ readTVar var
    putStrLn $ "Final value: " ++ show final

-- | Semaphore
data Semaphore = Semaphore 
    { permits :: TVar Int
    }

newSemaphore :: Int -> IO Semaphore
newSemaphore n = do
    p <- newTVarIO n
    return $ Semaphore p

waitSem :: Semaphore -> IO ()
waitSem sem = atomically $ do
    p <- readTVar (permits sem)
    if p > 0 
        then writeTVar (permits sem) (p - 1)
        else retry

signalSem :: Semaphore -> IO ()
signalSem sem = atomically $ do
    p <- readTVar (permits sem)
    writeTVar (permits sem) (p + 1)

-- | Main function
main :: IO ()
main = do
    putStrLn "=== Haskell Concurrency Demo ==="
    
    -- Basic fork
    putStrLn "\n--- Fork Example ---"
    forkExample
    
    -- Multiple threads
    putStrLn "\n--- Multiple Threads ---"
    threadExample 3
    
    -- STM examples
    putStrLn "\n--- STM Examples ---"
    stmExamples
    
    -- Async example
    putStrLn "\n--- Async Example ---"
    asyncExample
    
    -- Race condition
    putStrLn "\n--- Race Condition Demo ---"
    raceCondition
    
    -- Parallel quicksort
    putStrLn "\n--- Parallel QuickSort ---"
    let unsorted = [5, 2, 8, 1, 9, 3, 7, 4, 6]
    putStrLn $ "Unsorted: " ++ show unsorted
    putStrLn $ "Sorted: " ++ show (parQuickSort unsorted)
    
    putStrLn "\n=== Concurrency Demo Complete ==="

