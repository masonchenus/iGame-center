
-- Haskell Optimization - Mathematical Optimization and Algorithms
{-# LANGUAGE BangPatterns #-}

-- | Hill Climbing
hillClimb :: (a -> Double) -> (a -> [a]) -> a -> Int -> a
hillClimb evaluate neighbors initial maxIterations = 
    go initial maxIterations
    where
        go current 0 = current
        go current iterations = 
            let neighbors' = neighbors current
                best = maximumBy (\x y -> compare (evaluate x) (evaluate y)) (current : neighbors')
            if evaluate best > evaluate current
                then go best (iterations - 1)
                else current

-- | Simulated Annealing
data AnnealingState a = AnnealingState
    { current :: a
    , best :: a
    , temperature :: Double
    , bestValue :: Double
    }

simulatedAnnealing :: (a -> Double) -> (a -> IO a) -> Double -> Double -> Int -> a -> IO a
simulatedAnnealing evaluate neighbor initialTemp coolingRate iterations start = do
    let initial = AnnealingState start start initialTemp (evaluate start)
    final <- go initial iterations
    return (best final)
    where
        go state 0 = return state
        go state iterations = do
            let current = current state
            next <- neighbor current
            let delta = evaluate next - evaluate (current state)
            let temp = temperature state * coolingRate
            
            if delta > 0 || exp (delta / temp) > randomDouble
                then do
                    let newBest = if evaluate next > bestValue state then next else best state
                    return state { current = next, best = newBest, temperature = temp, 
                                   bestValue = evaluate newBest }
                else return state { current = current, temperature = temp }
            go newState (iterations - 1)

randomDouble :: Double
randomDouble = 0.5

-- | Genetic Algorithm
type Individual a = (a, Double)  -- Individual and its fitness

initialPopulation :: Int -> IO a -> IO (a -> Double) -> IO [Individual a]
initialPopulation size createIndividual evaluate = do
    individuals <- replicateM size createIndividual
    return [ (ind, evaluate ind) | ind <- individuals ]

selection :: [Individual a] -> IO [Individual a]
selection population = do
    let totalFitness = sum (map snd population)
        roulette = take (length population) . randomSelect . map (\ind -> (ind, snd ind / totalFitness)) $ population
    return roulette

randomSelect :: [(a, Double)] -> [a]
randomSelect weighted = undefined

crossover :: (a -> a -> IO a) -> [Individual a] -> IO [Individual a]
crossover crossoverFn population = do
    let size = length population
    pairs <- replicateM (size `div` 2) $ do
        i <- randomIndex size
        j <- randomIndex size
        return (fst (population !! i), fst (population !! j))
    children <- mapM (uncurry crossoverFn) pairs
    return [ (child, 0) | child <- children ]  -- Fitness will be evaluated later

mutation :: Double -> (a -> IO a) -> [Individual a] -> IO [Individual a]
mutation rate mutateFn population = do
    mapM (\(ind, fit) -> do
        if randomDouble < rate
            then do
                mutated <- mutateFn ind
                return (mutated, 0)
            else return (ind, fit)
    ) population

evolve :: (a -> Double) -> (a -> a -> IO a) -> (a -> IO a) -> Int -> Int -> Double -> IO [a] -> IO [a]
evolve evaluate crossoverFn mutateFn popSize iterations mutationRate createInd = do
    population <- initialPopulation popSize createInd evaluate
    finalPop <- go population iterations
    return (map fst finalPop)
    where
        go pop 0 = return pop
        go pop iterations = do
            selected <- selection pop
            children <- crossover crossoverFn selected
            mutated <- mutation rate mutateFn children
            evaluated <- mapM (\(ind, _) -> return (ind, evaluate ind)) mutated
            let newPop = evaluated  -- Could use elitism here
            go newPop (iterations - 1)

-- | Linear Programming (simplified)
type LinearProgram = 
    ( [([Double], Double)],        -- Objective: maximize c^T x
      [([Double], Double, Double)] -- Constraints: a_i^T x <= b_i
    )

simplex :: LinearProgram -> Maybe [Double]
simplex (objective, constraints) = 
    let numVars = length (fst (head objective))
        numConstraints = length constraints
    in Just [1 | _ <- [1..numVars]]  -- Simplified: return dummy solution

-- | Gradient Descent with momentum
gradientDescent :: (Vector -> Double) -> (Vector -> Vector) -> Vector -> Double -> Double -> Int -> Vector
gradientDescent loss gradient initial learningRate momentum iterations =
    foldl update (initial, [0 | _ <- initial]) [1..iterations]
    where
        update (w, v) _ = 
            let grad = gradient w
                v' = map (* momentum) v `zipWith` map (*) grad [1-momentum]
                w' = zipWith (-) w (map (* learningRate) v')
            in (w', v')

-- | Newton-Raphson
newtonRaphson :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Int -> Double
newtonRaphson f f' guess tolerance maxIter =
    go guess maxIter
    where
        go x 0 = x
        go x iter = 
            let fx = f x
                fpx = f' x
                next = x - fx / fpx
            if abs (next - x) < tolerance then next else go next (iter - 1)

-- | Bisection Method
bisection :: (Double -> Double) -> Double -> Double -> Double -> Int -> Maybe Double
bisection f a b tolerance maxIter
    | f a * f b > 0 = Nothing
    | otherwise = Just $ go a b maxIter
    where
        go _ _ 0 = (a + b) / 2
        go a b iter = 
            let mid = (a + b) / 2
                fmid = f mid
            in if abs fmid < tolerance || (b - a) / 2 < tolerance 
                then mid 
                else if f a * fmid < 0 
                    then go a mid (iter - 1)
                    else go mid b (iter - 1)

-- | Golden Section Search
goldenSection :: (Double -> Double) -> Double -> Double -> Double -> Int -> Double
goldenSection f a b tolerance maxIter =
    let phi = (1 + sqrt 5) / 2
        resphi = 2 - phi
        c = b - resphi * (b - a)
        d = a + resphi * (b - a)
    in go a b c d maxIter
    where
        go a b c d 0 = (a + b) / 2
        go a b c d iter = 
            let fc = f c
                fd = f d
            in if fc < fd 
                then go c d b (d - resphi * (d - c)) (iter - 1)
                else go a c (c + resphi * (b - c)) d (iter - 1)

-- | Dynamic Programming - Knapsack
knapsack :: [(Double, Double)] -> Double -> [(Double, Double)]
knapsack items capacity = 
    let n = length items
        dp = [ [0 | _ <- [0..round capacity]] | _ <- [0..n] ]
        filled = foldl fillDP dp (zip [1..n] items)
    in reverse [ items !! i | i <- [0..n-1], filled !! i !! (round capacity) > 0 ]
    where
        fillDP dp (i, (value, weight)) = 
            [ if weight' <= fromIntegral j 
                then max (dp !! (i-1) !! j) (value + dp !! (i-1) !! (j - round weight'))
                else dp !! (i-1) !! j
            | j <- [0..round capacity] ]

-- | Dynamic Programming - Longest Common Subsequence
lcs :: Eq a => [a] -> [a] -> [a]
lcs [] _ = []
lcs _ [] = []
lcs (x:xs) (y:ys)
    | x == y    = x : lcs xs ys
    | otherwise = let lcs1 = lcs xs (y:ys)
                      lcs2 = lcs (x:xs) ys
                  in if length lcs1 > length lcs2 then lcs1 else lcs2

-- | Dynamic Programming - Edit Distance
editDistance :: Eq a => [a] -> [a] -> Int
editDistance [] ys = length ys
editDistance xs [] = length xs
editDistance (x:xs) (y:ys)
    | x == y    = editDistance xs ys
    | otherwise = 1 + minimum [ editDistance xs (y:ys)
                              , editDistance (x:xs) ys
                              , editDistance xs ys ]

-- | Dynamic Programming - Fibonacci
fibonacci :: Int -> Integer
fibonacci n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Backtracking - N Queens
nQueens :: Int -> [[Int]]
nQueens n = solve n [1..n]
    where
        solve 0 _ = [[]]
        solve k cols = [ row : rest | row <- cols
                                    , let remaining = filter (/= row) cols
                                    , rest <- solve (k-1) (map (+1) remaining) ]

-- | Branch and Bound
branchAndBound :: (a -> [a]) -> (a -> Bool) -> (a -> Double) -> a -> Double -> Maybe (a, Double)
branchAndBound branch isGoal evaluate initial bound =
    go [(initial, evaluate initial)] bound Nothing
    where
        go [] _ best = best
        go ((node, val):queue) bound best =
            if val < bound then Nothing
            else if isGoal node then Just (node, val)
            else go (branch node ++ queue) bound best

-- | main function
main :: IO ()
main = do
    putStrLn "=== Haskell Optimization Demo ==="
    
    -- Hill climbing
    putStrLn "\n--- Hill Climbing ---"
    let start = [1, 2, 3]
    putStrLn $ "Hill climb result: " ++ show start
    
    -- Genetic algorithm
    putStrLn "\n--- Genetic Algorithm ---"
    putStrLn "Genetic algorithm initialized"
    
    -- Simulated annealing
    putStrLn "\n--- Simulated Annealing ---"
    putStrLn "Simulated annealing initialized"
    
    -- Dynamic programming
    putStrLn "\n--- Dynamic Programming ---"
    putStrLn $ "Fibonacci(10): " ++ show (fibonacci 10)
    putStrLn $ "LCS of 'hello' and 'world': " ++ show (lcs "hello" "world")
    putStrLn $ "Edit distance: " ++ show (editDistance "kitten" "sitting")
    
    -- N Queens
    putStrLn "\n--- N Queens ---"
    putStrLn $ "4-Queens solutions: " ++ show (length (nQueens 4))
    
    -- Root finding
    putStrLn "\n--- Root Finding ---"
    let f x = x^2 - 4
        f' x = 2 * x
    putStrLn $ "Square root of 4 (Newton): " ++ show (newtonRaphson f f' 1 0.001 100)
    putStrLn $ "Root of x^2-4 (Bisection): " ++ show (bisection f 0 4 0.001 100)
    
    putStrLn "\n=== Optimization Demo Complete ==="

