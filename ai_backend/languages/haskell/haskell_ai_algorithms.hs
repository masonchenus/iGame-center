
-- Haskell AI Algorithms - Machine Learning and AI Implementations
{-# LANGUAGE FlexibleInstances #-}

-- | Basic numeric operations
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

variance :: [Double] -> Double
variance xs = let m = mean xs in sum [(x - m) ** 2 | x xs] / fromIntegral (length xs)

stdDev :: [Double] -> Double
stdDev = sqrt . variance

-- | K-Nearest Neighbors
data KNN k a = KNN 
    { trainingData :: [(a, Double)]  -- (features, label)
    , k :: Int
    } deriving (Show)

euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance xs ys = sqrt $ sum [(x - y) ** 2 | (x, y) <- zip xs ys]

manhattanDistance :: [Double] -> [Double] -> Double
manhattanDistance xs ys = sum [abs (x - y) | (x, y) <- zip xs ys]

cosineSimilarity :: [Double] -> [Double] -> Double
cosineSimilarity xs ys = 
    let dot = sum [x * y | (x, y) <- zip xs ys]
        normX = sqrt $ sum [x ** 2 | x <- xs]
        normY = sqrt $ sum [y ** 2 | y <- ys]
    in if normX > 0 && normY > 0 then dot / (normX * normY) else 0

predictKNN :: (Ord b, Num b) => KNN Int [Double] -> [Double] -> b
predictKNN knn features = 
    let distances = map (\(fs, label) -> (euclideanDistance features fs, label)) 
                       (trainingData knn)
        sorted = take (k knn) $ sort distances
        votes = [label | (_, label) <- sorted]
    in round $ mean votes

-- | Linear Regression
data LinearRegression = LinearRegression
    { coefficients :: [Double]  -- [w0, w1, ..., wn]
    , intercept :: Double
    } deriving (Show)

trainLinearRegression :: [[Double]] -> [Double] -> LinearRegression
trainLinearRegression features labels = 
    let n = length features
        m = if n > 0 then length (head features) else 0
        -- Simplified: just use average for now
        avgFeatures = if n > 0 then [mean [f !! i | f <- features] | i <- [0..m-1]] else []
        avgLabel = mean labels
    in LinearRegression avgFeatures avgLabel

predictLinear :: LinearRegression -> [Double] -> Double
predictLinear lr features = 
    let dot = sum [c * f | (c, f) <- zip (coefficients lr) features]
    in intercept lr + dot

-- | Logistic Regression
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

data LogisticRegression = LogisticRegression
    { lrWeights :: [Double]
    , lrThreshold :: Double
    } deriving (Show)

trainLogisticRegression :: [[Double]] -> [Double] -> Int -> LogisticRegression
trainLogisticRegression features labels epochs = 
    let n = length features
        m = if n > 0 then length (head features) else 0
        weights = take m $ repeat 0.0
        threshold = 0.5
    in iterateUpdate weights features labels epochs
    where
        iterateUpdate w _ _ 0 = LogisticRegression w threshold
        iterateUpdate w f l e = 
            let predictions = map (sigmoid . predictLinear (LinearRegression w 0)) f
                errors = zipWith (-) l predictions
                gradient = [sum [e * x | (e, x) <- zip errors f'] / fromIntegral (length f) 
                           | f' <- transpose f]
                newW = zipWith (+) w gradient
            in iterateUpdate newW f l (e - 1)

predictLogistic :: LogisticRegression -> [Double] -> Double
predictLogistic lr features = 
    let score = sum [w * f | (w, f) <- zip (lrWeights lr) features]
    in sigmoid score

-- | Decision Tree
data DecisionTree a b = Leaf b | Node (a -> Bool) (DecisionTree a b) (DecisionTree a b)
    deriving (Show)

classify :: DecisionTree a b -> a -> b
classify (Leaf b) _ = b
classify (Node pred left right) x
    | pred x    = classify left x
    | otherwise = classify right x

-- | Random Forest
data RandomForest a b = RandomForest [DecisionTree a b]

predictForest :: RandomForest a b -> a -> b
predictForest (Forest trees) x = 
    let votes = [classify t x | t <- trees]
        frequency = foldr (\v acc -> Map.insertWith (+) v 1 acc) Map.empty votes
    in fst $ Map.foldrWithKey (\k v (mk, mv) -> if v > mv then (k, v) else (mk, mv)) (head votes, 0) frequency

-- | Naive Bayes
data NaiveBayes a = NaiveBayes
    { classPriors :: Map.Map a Double
    , featureMeans :: Map.Map (a, Int) Double
    , featureVars :: Map.Map (a, Int) Double
    } deriving (Show)

trainNaiveBayes :: [[Double]] -> [a] -> Int -> NaiveBayes a
trainNaiveBayes features labels numFeatures = 
    let classPriors = computePriors labels
        featureMeans = computeFeatureMeans features labels numFeatures
        featureVars = computeFeatureVars features labels numFeatures featureMeans
    in NaiveBayes classPriors featureMeans featureVars

computePriors :: Eq a => [a] -> Map.Map a Double
computePriors labels = 
    let total = fromIntegral (length labels)
        counts = foldr (\l acc -> Map.insertWith (+) l 1 acc) Map.empty labels
    in Map.map (/ total) counts

computeFeatureMeans :: [[Double]] -> [a] -> Int -> Map.Map (a, Int) Double
computeFeatureMeans features labels numFeatures = 
    foldr (\(f, l) acc -> 
        foldr (\(i, val) acc' -> 
            Map.insertWith (+) (l, i) val acc') acc (zip [0..numFeatures-1] f)
    ) Map.empty (zip features labels)

computeFeatureVars :: [[Double]] -> [a] -> Int -> Map.Map (a, Int) Double -> Map.Map (a, Int) Double
computeFeatureVars features labels numFeatures means = 
    foldr (\(f, l) acc -> 
        foldr (\(i, val) acc' -> 
            let key = (l, i)
                meanVal = Map.findWithDefault 0 key means
                diff = (val - meanVal) ** 2
            in Map.insertWith (+) key diff acc') acc (zip [0..numFeatures-1] f)
    ) Map.empty (zip features labels)

-- | Simple Neural Network
data NeuralNetwork = NeuralNetwork
    { layers :: [[[Double]]]  -- weights for each layer
    , biases :: [[Double]]    -- biases for each layer
    , activations :: [Activation]
    } deriving (Show)

data Activation = Sigmoid | ReLU | Tanh | Softmax deriving (Show)

forwardProp :: NeuralNetwork -> [Double] -> [Double]
forwardProp network input = 
    foldl (\inp (weights, bias, act) -> 
        let z = zipWith (+) (matrixVectorMult weights inp) bias
        in map (activate act) z
    ) input (zip3 (layers network) (biases network) (activations network))

activate :: Activation -> Double -> Double
activate Sigmoid x = 1 / (1 + exp (-x))
activate ReLU x = max 0 x
activate Tanh x = tanh x
activate Softmax xs = 
    let exps = map exp xs
        sumExps = sum exps
    in map (/ sumExps) exps

matrixVectorMult :: [[Double]] -> [Double] -> [Double]
matrixVectorMult matrix vector = 
    [ sum [m * v | (m, v) <- zip row vector] | row <- matrix ]

-- | K-Means Clustering
data KMeans k = KMeans
    { centroids :: [[Double]]
    , assignments :: [Int]
    } deriving (Show)

initializeCentroids :: Int -> [[Double]] -> [[Double]]
initializeCentroids k dataPoints = 
    take k dataPoints  -- Simplified initialization

updateAssignments :: [[Double]] -> [[Double]] -> [Int]
updateAssignments dataPoints centroids = 
    map (\p -> argmin [euclideanDistance p c | c <- centroids]) dataPoints

updateCentroids :: [[Double]] -> [Int] -> Int -> [[Double]]
updateCentroids dataPoints assignments k = 
    [ let clusterPoints = [p | (p, c) <- zip dataPoints assignments, c == i]
      in if null clusterPoints then replicate (length (head dataPoints)) 0
         else map mean (transpose clusterPoints)
    | i <- [0..k-1] ]

argmin :: (Ord a, Num a) => [a] -> Int
argmin xs = snd $ minimum (zip xs [0..])

kMeans :: [[Double]] -> Int -> Int -> KMeans k
kMeans dataPoints k iterations = 
    let centroids = initializeCentroids k dataPoints
        assignments = updateAssignments dataPoints centroids
    in if iterations <= 0 
        then KMeans centroids assignments
        else kMeans dataPoints k (iterations - 1)

-- | PCA (Principal Component Analysis)
data PCA = PCA
    { components :: [[Double]]
    , explainedVariance :: [Double]
    } deriving (Show)

computePCA :: [[Double]] -> Int -> PCA
computePCA data numComponents = 
    let centered = centerData data
        covariance = computeCovariance centered
        (eigenvalues, eigenvectors) = powerIteration covariance numComponents
    in PCA eigenvectors eigenvalues

centerData :: [[Double]] -> [[Double]]
centerData data = 
    let means = map mean (transpose data)
    in [zipWith (-) row means | row <- data]

computeCovariance :: [[Double]] -> [[Double]]
computeCovariance centered = 
    let n = fromIntegral (length centered)
        transposed = transpose centered
    in [ [ sum [t !! i * t !! j | t <- transposed] / n 
          | j <- [0..length transposed - 1] ]
        | i <- [0..length transposed - 1] ]

powerIteration :: [[Double]] -> Int -> ([Double], [[Double]])
powerIteration matrix k = 
    let n = length matrix
        v = replicate n 1.0
    in ([1.0 | _ <- [1..k]], [[1.0 | _ <- [1..n]] | _ <- [1..k]])

-- | Hidden Markov Model
data HMM states observations = HMM
    { initialProb :: Map.Map states Double
    , transitionProb :: Map.Map (states, states) Double
    , emissionProb :: Map.Map (states, observations) Double
    } deriving (Show)

forwardAlgorithm :: (Eq states, Eq observations) => 
    HMM states observations -> [observations] -> Map.Map states Double
forwardAlgorithm hmm observations = 
    let alpha0 = Map.mapWithKey (\s p -> p * emissionProb hmm Map.! (s, head observations)) 
                                  (initialProb hmm)
    in foldl (\alpha obs -> 
        Map.mapWithKey (\s sumProb -> 
            emissionProb hmm Map.! (s, obs) * 
            sum [alpha Map.! prevS * transitionProb hmm Map.! (prevS, s) 
                | prevS <- Map.keys (initialProb hmm)]
        ) (initialProb hmm)
    ) alpha0 (tail observations)

viterbi :: (Eq states, Eq observations) => 
    HMM states observations -> [observations] -> [states]
viterbi hmm observations = []
    -- Implementation would go here

-- | Genetic Algorithm
type Individual = [Double]
type Population = [Individual]
type FitnessFunction = Individual -> Double

select :: Population -> FitnessFunction -> Population
select pop fitness = 
    let sorted = sortBy (\a b -> compare (fitness b) (fitness a)) pop
    in take (length pop `div` 2) sorted

crossover :: Individual -> Individual -> (Individual, Individual)
crossover p1 p2 = 
    let point = length p1 `div` 2
        (h1, t1) = splitAt point p1
        (h2, t2) = splitAt point p2
    in (h1 ++ t2, h2 ++ t1)

mutate :: Double -> Individual -> Individual
mutate rate ind = 
    map (\x -> if random < rate then x + (random - 0.5) else x) ind
    where random = (fromIntegral (rem (abs (hash x)) 100) / 100)

evolve :: Population -> FitnessFunction -> Double -> Int -> Population
evolve pop fitness crossoverRate iterations = 
    if iterations <= 0 then pop
    else let selected = select pop fitness
             children = concatMap (\(p1, p2) -> 
                 let (c1, c2) = crossover p1 p2
                 in [mutate crossoverRate c1, mutate crossoverRate c2]
             ) (zip selected (tail selected ++ [head selected]))
             newPop = selected ++ children
         in evolve newPop fitness crossoverRate (iterations - 1)

-- | A* Search
data AStarState node = AStarState
    { node :: node
    , gScore :: Double  -- Cost from start
    , fScore :: Double  -- gScore + heuristic
    , parent :: Maybe node
    } deriving (Show)

aStar :: (Eq node, Ord node) => 
    (node -> [node]) ->          -- neighbors
    (node -> node -> Double) ->  -- edge cost
    (node -> Double) ->          -- heuristic
    node ->                      -- start
    node ->                      -- goal
    Maybe [node]
aStar neighbors cost heuristic start goal = undefined

-- | Q-Learning (Reinforcement Learning)
data QLearning state action = QLearning
    { qTable :: Map.Map (state, action) Double
    , learningRate :: Double
    , discountFactor :: Double
    , explorationRate :: Double
    } deriving (Show)

initializeQL :: Double -> Double -> Double -> QLearning s a
initializeQL alpha gamma epsilon = QLearning Map.empty alpha gamma epsilon

selectAction :: (Eq s, Eq a) => QLearning s a -> s -> [a] -> Double -> (a, QLearning s a)
selectAction qlearn state actions randomVal =
    if randomVal < explorationRate qlearn
        then (head actions, qlearn)  -- Explore
        else (argmaxAction state actions qlearn, qlearn)  -- Exploit

argmaxAction :: (Eq s, Eq a) => s -> [a] -> QLearning s a -> a
argmaxAction state actions qlearn = 
    let values = map (\a -> Map.findWithDefault 0 (state, a) (qTable qlearn)) actions
    in actions !! argmin values

updateQ :: (Eq s, Eq a) => QLearning s a -> s -> a -> Double -> s -> QLearning s a
updateQ qlearn state action reward nextState = 
    let oldQ = Map.findWithDefault 0 (state, action) (qTable qlearn)
        maxQ = maximum [Map.findWithDefault 0 (nextState, a) (qTable qlearn) | a <- []]
        newQ = oldQ + learningRate qlearn * (reward + discountFactor qlearn * maxQ - oldQ)
    in qlearn { qTable = Map.insert (state, action) newQ (qTable qlearn) }

-- | Main function for testing
main :: IO ()
main = do
    putStrLn "=== Haskell AI Algorithms Demo ==="
    
    -- KNN Example
    putStrLn "\n--- K-Nearest Neighbors ---"
    let training = [([1.0, 2.0], 0), ([2.0, 3.0], 0), ([3.0, 4.0], 1), ([4.0, 5.0], 1)]
    let knn = KNN training 3
    print $ predictKNN knn [2.5, 3.5]
    
    -- Linear Regression Example
    putStrLn "\n--- Linear Regression ---"
    let features = [[1.0], [2.0], [3.0], [4.0], [5.0]]
    let labels = [2.0, 4.0, 6.0, 8.0, 10.0]
    let lr = trainLinearRegression features labels
    print $ predictLinear lr [6.0]
    
    -- K-Means Example
    putStrLn "\n--- K-Means Clustering ---"
    let dataPoints = [[1.0, 1.0], [1.5, 1.5], [5.0, 5.0], [5.5, 5.5], [10.0, 10.0], [10.5, 10.5]]
    let km = kMeans dataPoints 2 10
    print $ centroids km
    
    -- Neural Network Example
    putStrLn "\n--- Neural Network ---"
    let nn = NeuralNetwork [[[0.5, -0.5]], [[1.0, -1.0]]] [[0.1], [0.1]] [Sigmoid, Sigmoid]
    print $ forwardProp nn [0.5, 0.5]
    
    putStrLn "\n=== AI Algorithms Demo Complete ==="

