
-- Haskell ML Core - Machine Learning Core Functions and Utilities
{-# LANGUAGE FlexibleInstances #-}

-- | Matrix operations
type Matrix = [[Double]]

matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply a b = 
    let colsB = transpose b
    in [ [ sum (zipWith (*) row col) | col <- colsB ] | row <- a ]

matrixAdd :: Matrix -> Matrix -> Matrix
matrixAdd = zipWith (zipWith (+))

matrixSubtract :: Matrix -> Matrix -> Matrix
matrixSubtract = zipWith (zipWith (-))

matrixScalarMultiply :: Double -> Matrix -> Matrix
matrixScalarMultiply k = map (map (* k))

matrixTranspose :: Matrix -> Matrix
matrixTranspose [] = []
matrixTranspose (x:xs) = zipWith (:) x (matrixTranspose xs)

matrixIdentity :: Int -> Matrix
matrixIdentity n = [ [ if i == j then 1.0 else 0.0 | j <- [1..n] ] | i <- [1..n] ]

matrixDeterminant :: Matrix -> Double
matrixDeterminant [[x]] = x
matrixDeterminant m = 
    let n = length m
    in sum [ (-1)^i * m !! 0 !! i * matrixDeterminant (removeRowCol 0 i m) | i <- [0..n-1] ]

removeRowCol :: Int -> Int -> Matrix -> Matrix
removeRowCol row col m = 
    let m' = removeRow row m
    in map (removeCol col) m'

removeRow :: Int -> Matrix -> Matrix
removeRow i m = take i m ++ drop (i + 1) m

removeCol :: Int -> [a] -> [a]
removeCol i row = take i row ++ drop (i + 1) row

-- | Vector operations
type Vector = [Double]

vectorDot :: Vector -> Vector -> Double
vectorDot = sum .: zipWith (*)
    where (.:) f g x y = f (g x y)

vectorMagnitude :: Vector -> Double
vectorMagnitude = sqrt . sum . map (^ 2)

vectorNormalize :: Vector -> Vector
vectorNormalize v = let mag = vectorMagnitude v in map (/ mag) v

vectorCosineSimilarity :: Vector -> Vector -> Double
vectorCosineSimilarity v1 v2 = vectorDot v1 v2 / (vectorMagnitude v1 * vectorMagnitude v2)

-- | Activation functions
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = let s = sigmoid x in s * (1 - s)

relu :: Double -> Double
relu x = max 0 x

relu' :: Double -> Double
relu' x = if x > 0 then 1 else 0

tanh' :: Double -> Double
tanh' x = 1 - tanh x ^ 2

leakyRelu :: Double -> Double
leakyRelu x = if x > 0 then x else 0.01 * x

leakyRelu' :: Double -> Double
leakyRelu' x = if x > 0 then 1 else 0.01

softmax :: [Double] -> [Double]
softmax xs = 
    let exps = map exp xs
        sumExps = sum exps
    in map (/ sumExps) exps

-- | Loss functions
meanSquaredError :: [Double] -> [Double] -> Double
meanSquaredError predicted actual = 
    let n = fromIntegral (length predicted)
        squaredErrors = zipWith (\p a -> (p - a) ^ 2) predicted actual
    in sum squaredErrors / n

crossEntropy :: [Double] -> [Double] -> Double
crossEntropy predicted actual = 
    let logPred = zipWith (\p a -> if p > 0 then -log p * a else 0) predicted actual
    in sum logPred

binaryCrossEntropy :: [Double] -> [Double] -> Double
binaryCrossEntropy predicted actual =
    let n = fromIntegral (length predicted)
        errors = zipWith (\p a -> -a * log p - (1 - a) * log (1 - p)) predicted actual
    in sum errors / n

-- | Gradient descent
gradientDescent :: (Vector -> Vector -> Double) -> (Vector -> Vector -> Vector) -> 
                  Vector -> [Vector] -> Int -> Double -> [Vector]
gradientDescent lossFunction gradientFunction initial weights learningRate iterations =
    iterate updateWeight weights !! iterations
    where
        updateWeight w = zipWith (-) w (map (* learningRate) (gradientFunction w (head weights)))

-- | Stochastic gradient descent
sgd :: (Vector -> Vector -> Double) -> (Vector -> Vector -> Vector) -> 
     Vector -> [(Vector, Vector)] -> Int -> Double -> Vector
sgd loss gradient initial data iterations lr =
    foldl (\w (x, y) -> zipWith (-) w (map (* lr) (gradient w x))) initial data

-- | Momentum
momentum :: (Vector -> Vector -> Double) -> (Vector -> Vector -> Vector) -> 
          Vector -> [(Vector, Vector)] -> Int -> Double -> Double -> Vector
momentum loss gradient initial data iterations lr momentum =
    foldl update (initial, [0 | _ <- initial]) data !! iterations
    where
        update (w, v) (x, y) = 
            let grad = gradient w x
                v' = map (\(g, vi) -> momentum * vi + (1 - momentum) * g) (zip grad v)
                w' = zipWith (-) w (map (* lr) v')
            in (w', v')

-- | Adam optimizer
adam :: (Vector -> Vector -> Double) -> (Vector -> Vector -> Vector) ->
      Vector -> [(Vector, Vector)] -> Int -> Double -> Double -> Double -> Double -> Vector
adam loss gradient initial data iterations lr beta1 beta2 epsilon =
    foldl update (initial, m0, v0, 0) data !! iterations
    where
        m0 = replicate (length initial) 0
        v0 = replicate (length initial) 0
        t = 0
        update (w, m, v, t) (x, y) = (w', m', v', t + 1)
            where
                grad = gradient w x
                t' = t + 1
                m' = zipWith (\m g -> beta1 * m + (1 - beta1) * g) m grad
                v' = zipWith (\v g -> beta2 * v + (1 - beta2) * g * g) v grad
                mHat = map (/ (1 - beta1 ** t')) m'
                vHat = map (/ (1 - beta2 ** t')) v'
                w' = zipWith (\w m v -> w - lr * m / (sqrt v + epsilon)) w mHat vHat

-- | Regularization
l2Regularization :: Double -> Matrix -> Double
l2Regularization lambda weights = 
    let flattened = concat weights
    in lambda * sum (map (^ 2) flattened) / 2

l1Regularization :: Double -> Matrix -> Double
l1Regularization lambda weights = 
    lambda * sum (map abs (concat weights))

elasticNet :: Double -> Double -> Matrix -> Double
elasticNet lambda1 lambda2 weights = 
    l1Regularization lambda1 weights + l2Regularization lambda2 weights

-- | Normalization
minMaxNormalize :: Vector -> Vector
minMaxNormalize v = 
    let minV = minimum v
        maxV = maximum v
    in map (\x -> (x - minV) / (maxV - minV)) v

zScoreNormalize :: Vector -> Vector
zScoreNormalize v = 
    let meanV = sum v / fromIntegral (length v)
        stdV = sqrt (sum (map (\x -> (x - meanV) ^ 2) v) / fromIntegral (length v))
    in map (\x -> (x - meanV) / stdV) v

standardScaler :: [Vector] -> (Vector -> Vector, Vector -> Vector)
standardScaler data = 
    let means = map mean (transpose data)
        stds = map std (transpose data)
        fit v = zipWith (\m s -> if s == 0 then 0 else (v !! i - m) / s) means stds
        transform v = zipWith (*) v (map (\s -> if s == 0 then 1 else s) stds)
    in (fit, transform)
    where
        mean v = sum v / fromIntegral (length v)
        std v = sqrt (sum (map (^ 2) v) / fromIntegral (length v)) - mean v

-- | One-hot encoding
oneHot :: Int -> Int -> Vector
oneHot n idx = [ if i == idx then 1.0 else 0.0 | i <- [0..n-1] ]

decodeOneHot :: Vector -> Int
decodeOneHot v = snd $ maximum (zip v [0..])

-- | Train-test split
trainTestSplit :: Double -> [a] -> ([a], [a])
trainTestSplit ratio data = 
    let n = length data
        trainSize = round (fromIntegral n * ratio)
    in (take trainSize data, drop trainSize data)

kFoldCrossValidation :: Int -> [a] -> [[a]]
kFoldCrossValidation k data = 
    let chunkSize = length data `div` k
    in [ take i (drop (i * chunkSize) data) | i <- [0..k-1] ]

-- | Evaluation metrics
accuracy :: [Int] -> [Int] -> Double
accuracy predicted actual = 
    let correct = length (filter (== True) (zipWith (==) predicted actual))
    in fromIntegral correct / fromIntegral (length predicted)

precision :: [Int] -> [Int] -> Double
precision predicted actual =
    let truePositives = length (filter (== True) (zipWith (\p a -> p == 1 && a == 1) predicted actual))
        predictedPositives = length (filter (== 1) predicted)
    in fromIntegral truePositives / fromIntegral predictedPositives

recall :: [Int] -> [Int] -> Double
recall predicted actual =
    let truePositives = length (filter (== True) (zipWith (\p a -> p == 1 && a == 1) predicted actual))
        actualPositives = length (filter (== 1) actual)
    in fromIntegral truePositives / fromIntegral actualPositives

f1Score :: [Int] -> [Int] -> Double
f1Score predicted actual = 
    let p = precision predicted actual
        r = recall predicted actual
    in 2 * p * r / (p + r)

confusionMatrix :: [Int] -> [Int] -> [[Int]]
confusionMatrix predicted actual = undefined

-- | main function
main :: IO ()
main = do
    putStrLn "=== Haskell ML Core Demo ==="
    
    -- Matrix operations
    putStrLn "\n--- Matrix Operations ---"
    let m1 = [[1, 2], [3, 4]] :: Matrix
        m2 = [[5, 6], [7, 8]] :: Matrix
    putStrLn $ "Matrix 1: " ++ show m1
    putStrLn $ "Matrix 2: " ++ show m2
    putStrLn $ "Multiplication: " ++ show (matrixMultiply m1 m2)
    putStrLn $ "Addition: " ++ show (matrixAdd m1 m2)
    
    -- Vector operations
    putStrLn "\n--- Vector Operations ---"
    let v1 = [1, 2, 3] :: Vector
        v2 = [4, 5, 6] :: Vector
    putStrLn $ "Vector 1: " ++ show v1
    putStrLn $ "Vector 2: " ++ show v2
    putStrLn $ "Dot product: " ++ show (vectorDot v1 v2)
    putStrLn $ "Cosine similarity: " ++ show (vectorCosineSimilarity v1 v2)
    
    -- Activation functions
    putStrLn "\n--- Activation Functions ---"
    putStrLn $ "Sigmoid(0): " ++ show (sigmoid 0)
    putStrLn $ "ReLU(-1): " ++ show (relu (-1))
    putStrLn $ "Softmax [1,2,3]: " ++ show (softmax [1,2,3])
    
    -- Loss functions
    putStrLn "\n--- Loss Functions ---"
    putStrLn $ "MSE: " ++ show (meanSquaredError [1,2,3] [1.1, 2.1, 2.9])
    putStrLn $ "Cross entropy: " ++ show (crossEntropy [0.9, 0.1] [1, 0])
    
    -- Normalization
    putStrLn "\n--- Normalization ---"
    let v = [1, 2, 3, 4, 5] :: Vector
    putStrLn $ "Min-max: " ++ show (minMaxNormalize v)
    putStrLn $ "Z-score: " ++ show (zScoreNormalize v)
    
    -- One-hot encoding
    putStrLn "\n--- One-Hot Encoding ---"
    putStrLn $ "One-hot(5, 2): " ++ show (oneHot 5 2)
    
    putStrLn "\n=== ML Core Demo Complete ==="

