
-- Haskell Parallel - Parallel and Distributed Computing
{-# LANGUAGE BangPatterns #-}

import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad
import Data.List
import System.Environment
import System.CPUTime
import Text.Printf

-- | Parallel list processing using parMap
parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f xs = parMap rpar f xs

-- | Parallel fold using divide and conquer
parFold :: (a -> a -> a) -> a -> [a] -> a
parFold f z [] = z
parFold f z [x] = f z x
parFold f z xs = 
    let (left, right) = splitAt (length xs `div` 2) xs
        leftResult = parFold f z left
        rightResult = parFold f z right
    in f leftResult `using` parList rseq

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
    `using` parList rseq

-- | Parallel merge sort
parMergeSort :: (Ord a) => [a] -> [a]
parMergeSort [] = []
parMergeSort [x] = [x]
parMergeSort xs = 
    let (left, right) = splitAt (length xs `div` 2) xs
        sortedLeft = parMergeSort left `using` parList rseq
        sortedRight = parMergeSort right `using` parList rseq
    in merge sortedLeft sortedRight
    where
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys)
            | x <= y    = x : merge xs (y:ys)
            | otherwise = y : merge (x:xs) ys

-- | Parallel matrix multiplication
parMatrixMultiply :: [[Double]] -> [[Double]] -> [[Double]]
parMatrixMultiply a b = 
    let bTransposed = transpose b
    in [ [ sum (zipWith (*) row col) | col <- bTransposed ] 
        | row <- a ] 
    `using` parList rpar

-- | Parallel dot product
parDotProduct :: [Double] -> [Double] -> Double
parDotProduct v1 v2 = 
    let products = [ x * y | (x, y) <- zip v1 v2 ] `using` parList rpar
    in sum products

-- | Parallel sum
parSum :: [Double] -> Double
parSum xs = 
    foldl (+) 0 (xs `using` parList rpar)

-- | Parallel maximum
parMaximum :: (Ord a) => [a] -> a
parMaximum xs = 
    maximum (xs `using` parList rseq)

-- | Parallel minimum
parMinimum :: (Ord a) => [a] -> a
parMinimum xs = 
    minimum (xs `using` parList rseq)

-- | Parallel sorting networks
oddEvenSort :: (Ord a) => [a] -> [a]
oddEvenSort xs = 
    go xs True
    where
        go xs True = go (oddPhase xs) False
        go xs False = if sorted xs then xs else go (evenPhase xs) True
        oddPhase [] = []
        oddPhase [x] = [x]
        oddPhase (x:y:xs) = min x y : max x y : oddPhase xs
        evenPhase [] = []
        evenPhase [x] = [x]
        evenPhase (x:y:xs) = min x y : max x y : evenPhase xs
        sorted xs = all (uncurry (<=)) (zip xs (tail xs))

-- | Parallel filter
parFilter :: (a -> Bool) -> [a] -> [a]
parFilter p xs = 
    [ x | x <- xs, p x ] `using` parList rseq

-- | Parallel map with chunking
parMapChunk :: Int -> (a -> b) -> [a] -> [b]
parMapChunk chunkSize f xs = 
    let chunks = chunk chunkSize xs
        results = map (map f) chunks `using` parList rpar
    in concat results
    where
        chunk _ [] = []
        chunk n xs = let (h, t) = splitAt n xs in h : chunk n t

-- | Parallel reductions
parReduce :: (a -> a -> a) -> a -> [a] -> a
parReduce f z [] = z
parReduce f z [x] = f z x
parReduce f z xs = 
    let (left, right) = splitAt (length xs `div` 2) xs
        leftResult = parReduce f z left
        rightResult = parReduce f z right
    in f leftResult rightResult

-- | Parallel prefix sum (scan)
parScan :: (a -> a -> a) -> a -> [a] -> [a]
parScan f z [] = []
parScan f z [x] = [f z x]
parScan f z xs = 
    let (left, right) = splitAt (length xs `div` 2) xs
        leftScan = parScan f z left
        rightScan = parScan f (foldl f z left) right
    in leftScan ++ rightScan

-- | Work stealing scheduler (simplified)
data WorkStealingQueue a = WorkStealingQueue
    { top :: Int
    , bottom :: Int
    , array :: [a]
    }

push :: a -> WorkStealingQueue a -> WorkStealingQueue a
push x queue = queue { array = x : array queue, top = top queue + 1 }

pop :: WorkStealingQueue a -> (Maybe a, WorkStealingQueue a)
pop queue = 
    let newArray = tail (array queue)
    in (Just (head (array queue)), queue { array = newArray })

-- | Parallel Fibonacci using divide and conquer
parallelFib :: Int -> Integer
parallelFib 0 = 0
parallelFib 1 = 1
parallelFib n = 
    let a = parallelFib (n - 1)
        b = parallelFib (n - 2)
    in a + b
    `using` parTuple2 rpar rpar

-- | Parallel tree traversal
data Tree a = Leaf a | Node (Tree a) (Tree a)

parTreeMap :: (a -> b) -> Tree a -> Tree b
parTreeMap f (Leaf x) = Leaf (f x)
parTreeMap f (Node left right) = 
    let left' = parTreeMap f left
        right' = parTreeMap f right
    in Node left' right'
    `using` parTuple2 rpar rpar

-- | Parallel image processing (conceptual)
type Image = [[Pixel]]
type Pixel = (Double, Double, Double)

parBlur :: Int -> Image -> Image
parBlur n image = 
    let blurred = map (blurRow n) image
    in blurred `using` parList rpar
    where
        blurRow n row = undefined

-- | Parallel document processing
type Document = String

parWordCount :: [Document] -> [(String, Int)]
parWordCount docs = 
    let counts = map wordCount docs `using` parList rpar
    in mergeCounts counts
    where
        wordCount doc = length (words doc)
        mergeCounts = foldl merge Map.empty
        merge acc count = Map.insertWith (+) (show count) 1 acc

-- | Parallel Monte Carlo simulation
monteCarlo :: Int -> Double -> IO Double
monteCarlo iterations threads = do
    let chunkSize = iterations `div` threads
    results <- replicateM threads (mcChunk chunkSize)
    return (sum results / fromIntegral threads)
    where
        mcChunk n = do
            hits <- countMCHits n
            return (fromIntegral hits / fromIntegral n)
        countMCHits n = 
            let xs = take n (randomDoubles ())
                ys = take n (randomDoubles ())
            in return (length (filter (\(x,y) -> x*x + y*y <= 1) (zip xs ys)))

randomDoubles :: [Double]
randomDoubles = undefined

-- | Benchmarking
timeIt :: IO a -> IO (a, Double)
timeIt action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^9)
    return (result, diff)

-- | Parallelism strategies
data EvaluationStrategy = Seq | Par | ParChunk Int

applyStrategy :: EvaluationStrategy -> [a] -> [a]
applyStrategy Seq xs = xs `using` evalList seq
applyStrategy Par xs = xs `using` parList rseq
applyStrategy (ParChunk n) xs = xs `using` parList rseq

-- | main function
main :: IO ()
main = do
    putStrLn "=== Haskell Parallel Demo ==="
    
    -- Benchmark sequential vs parallel
    putStrLn "\n--- Sequential vs Parallel ---"
    let data = [1..1000000] :: [Int]
    
    (result1, time1) <- timeIt $ return (sum data)
    putStrLn $ "Sequential sum: " ++ show result1 ++ " (time: " ++ printf "%.2f" time1 ++ "ms)"
    
    (result2, time2) <- timeIt $ return (parSum data)
    putStrLn $ "Parallel sum: " ++ show result2 ++ " (time: " ++ printf "%.2f" time2 ++ "ms)"
    
    -- Parallel sorting
    putStrLn "\n--- Parallel Sorting ---"
    let unsorted = [1000000, 999999..1] :: [Int]
    
    (sorted1, time3) <- timeIt $ return (parQuickSort unsorted)
    putStrLn $ "Parallel quicksort time: " ++ printf "%.2f" time3 ++ "ms"
    
    -- Parallel map
    putStrLn "\n--- Parallel Map ---"
    (mapped, time4) <- timeIt $ return (parallelMap (*2) data)
    putStrLn $ "Parallel map time: " ++ printf "%.2f" time4 ++ "ms"
    
    -- Fibonacci comparison
    putStrLn "\n--- Fibonacci Comparison ---"
    (fib35, time5) <- timeIt $ return (parallelFib 35)
    putStrLn $ "Parallel Fib(35): " ++ show fib35 ++ " (time: " ++ printf "%.2f" time5 ++ "ms)"
    
    -- Matrix operations
    putStrLn "\n--- Matrix Operations ---"
    let matrix = take 100 (repeat (take 100 [1..100])) :: [[Double]]
    
    (result, time6) <- timeIt $ return (parMap (parSum . map (*2)) matrix)
    putStrLn $ "Parallel matrix operations time: " ++ printf "%.2f" time6 ++ "ms"
    
    putStrLn "\n=== Parallel Demo Complete ==="

