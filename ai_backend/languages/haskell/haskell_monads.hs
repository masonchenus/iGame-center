
-- Haskell Monads - Functional Programming with Monads and Functors
-- This file demonstrates various monadic patterns in Haskell

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Basic Monad type class
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    m >> k = m >>= \_ -> k

-- | Functor type class
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- | Applicative type class  
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- | Maybe Monad - handling optional values
data Maybe a = Nothing | Just a deriving (Show, Eq)

instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> Just x = Just (f x)
    (Just f) <*> Nothing = Nothing

instance Monad Maybe where
    return = Just
    Nothing >>= _ = Nothing
    Just x >>= f = f x

-- | Either Monad - handling errors
data Either a b = Left a | Right b deriving (Show, Eq)

instance Functor (Either a) where
    fmap _ (Left x)  = Left x
    fmap f (Right y) = Right (f y)

instance Applicative (Either a) where
    pure = Right
    Left x <*> _ = Left x
    Right f <*> Right x = Right (f x)
    Right f <*> Left x = Left x

instance Monad (Either a) where
    return = Right
    Left x >>= _ = Left x
    Right x >>= f = f x

-- | List Monad - non-deterministic computation
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)

instance Applicative [] where
    pure = return
    fs <*> xs = concatMap (\f -> map f xs) fs

instance Functor [] where
    fmap = map

-- | State Monad - managing stateful computations
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State g) = State $ \s -> 
        let (a, s') = g s 
        in (f a, s')

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)
    (State f) <*> (State g) = State $ \s ->
        let (h, s') = f s
            (a, s'') = g s'
        in (h a, s'')

instance Monad (State s) where
    return = pure
    State f >>= k = State $ \s ->
        let (a, s') = f s
        in runState (k a) s'

-- | State Monad helper functions
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

-- | Reader Monad - reading from environment
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
    pure = Reader . const
    (Reader f) <*> (Reader g) = Reader $ \r -> f r (g r)

instance Monad (Reader r) where
    return = pure
    m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

ask :: Reader r r
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \r -> runReader m (f r)

runReaderT :: Reader r a -> r -> a
runReaderT = runReader

-- | Writer Monad - accumulating output
newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Functor (Writer w) where
    fmap f (Writer (a, w)) = Writer (f a, w)

instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    Writer (f, w1) <*> Writer (a, w2) = Writer (f a, w1 <> w2)

instance (Monoid w) => Monad (Writer w) where
    return = pure
    Writer (a, w) >>= k = 
        let (b, w') = runWriter (k a)
        in Writer (b, w <> w')

tell :: w -> Writer w ()
tell w = Writer ((), w)

runWriterT :: Writer w a -> (a, w)
runWriterT = runWriter

-- | IO Monad - input/output operations
instance Monad IO where
    return = pure
    m >>= k = k =<< m

-- | MaybeT Monad Transformer
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT m) = MaybeT $ fmap (fmap f) m

instance (Applicative m) => Applicative (MaybeT m) where
    pure a = MaybeT $ pure (Just a)
    MaybeT f <*> MaybeT m = MaybeT $ (<*>) <$> f <*> m

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    MaybeT m >>= k = MaybeT $ do
        ma <- m
        case ma of
            Nothing -> return Nothing
            Just a  -> runMaybeT (k a)

-- | StateT Monad Transformer
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT g) = StateT $ fmap (\(a, s) -> (f a, s)) . g

instance (Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \s -> return (a, s)
    StateT f <*> StateT g = StateT $ \s -> do
        (h, s') <- f s
        (a, s'') <- g s'
        return (h a, s'')

instance (Monad m) => Monad (StateT s m) where
    return = pure
    StateT m >>= k = StateT $ \s -> do
        (a, s') <- m s
        runStateT (k a) s'

-- | Continuation Monad
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
    fmap f (Cont g) = Cont $ \k -> g (k . f)

instance Applicative (Cont r) where
    pure a = Cont ($ a)
    (Cont f) <*> (Cont g) = Cont $ \k -> f (\h -> g (k . h))

instance Monad (Cont r) where
    return = pure
    Cont m >>= k = Cont $ \c -> m (\a -> runCont (k a) c

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \c -> runCont (f (\a -> Cont $ \_ -> c a)) c

-- | Custom data type with Monad instance
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Functor)

instance Applicative (Tree a) where
    pure = Leaf
    (Node f g) <*> (Node x y) = Node (f <*> x) (g <*> y)
    (Leaf f) <*> (Leaf x) = Leaf (f x)
    (Leaf f) <*> (Node x y) = Node (Leaf f <*> x) (Leaf f <*> y)
    (Node f g) <*> (Leaf x) = Node (f <*> (Leaf x)) (g <*> (Leaf x))

instance Monad Tree where
    return = Leaf
    (Leaf x) >>= f = f x
    (Node l r) >>= f = Node (l >>= f) (r >>= f)

-- | Maybe operations
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe x Nothing  = x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

-- | Either operations
safeDivE :: Double -> Double -> Either String Double
safeDivE _ 0 = Left "Division by zero"
safeDivE x y = Right (x / y)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

fromEither :: b -> Either a b -> b
fromEither _ (Right x) = x
fromEither y (Left _)  = y

-- | State examples
type Stack a = [a]

push :: a -> State (Stack a) ()
push x = modify (x:)

pop :: State (Stack a) (Maybe a)
pop = do
    (x:xs) <- get
    put xs
    return (Just x)
    <|> return Nothing

stackOps :: State (Stack Int) Int
stackOps = do
    push 1
    push 2
    push 3
    pop
    pop
    (Just x) <- pop
    return x

-- | Reader example with environment
data Config = Config 
    { host :: String
    , port :: Int
    , debug :: Bool
    } deriving (Show)

getHost :: Reader Config String
getHost = ask >>= \c -> return (host c)

getPort :: Reader Config Int
getPort = ask >>= \c -> return (port c)

isDebug :: Reader Config Bool
isDebug = ask >>= \c -> return (debug c)

withConfig :: Config -> Reader Config a -> a
withConfig = flip runReader

-- | Writer example with logging
type Log = [String]

logMsg :: String -> Writer Log ()
logMsg = tell . (:[])

processData :: [Int] -> Writer Log Int
processData xs = do
    logMsg $ "Processing " ++ show (length xs) ++ " items"
    let result = sum xs
    logMsg $ "Sum is " ++ show result
    return result

-- | Do notation examples
example1 :: Maybe Int
example1 = do
    x <- Just 3
    y <- Just 4
    return (x + y)

example2 :: Either String Int
example2 = do
    x <- Right 3
    y <- Right 4
    if x + y > 10
        then Left "Sum too large"
        else Right (x + y)

example3 :: State Int String
example3 = do
    modify (+1)
    n <- get
    return ("State is now " ++ show n)

-- | Monad laws demonstration
leftIdentity :: Maybe Int
leftIdentity = return 3 >>= \x -> Just (x * 2)

rightIdentity :: Maybe Int
rightIdentity = Just 3 >>= return

associativity :: Maybe Int
associativity = Just 3 >>= \x -> Just (x + 1) >>= \y -> Just (y * 2)

-- | List comprehension as monad
cartesian :: [(Int, Char)]
cartesian = do
    x <- [1, 2, 3]
    y <- ['a', 'b']
    return (x, y)

-- | Main function for testing
main :: IO ()
main = do
    putStrLn "=== Haskell Monads Demo ==="
    
    -- Maybe examples
    putStrLn "\n--- Maybe Monad ---"
    print $ fmap (*2) (Just 5)
    print $ Just 3 >>= \x -> Just (x + 4)
    print $ safeDiv 10 2
    print $ safeDiv 10 0
    
    -- Either examples  
    putStrLn "\n--- Either Monad ---"
    print $ Right 3 >>= \x -> Right (x * 2)
    print $ safeDivE 10 2
    print $ safeDivE 10 0
    
    -- List examples
    putStrLn "\n--- List Monad ---"
    print $ [1,2,3] >>= \x -> [x, x*2]
    print cartesian
    
    -- State examples
    putStrLn "\n--- State Monad ---"
    let result = evalState stackOps []
    print result
    
    -- Reader examples
    putStrLn "\n--- Reader Monad ---"
    let config = Config "localhost" 8080 True
    let host = runReader getHost config
    print host
    
    -- Writer examples
    putStrLn "\n--- Writer Monad ---"
    let (sum, logs) = runWriter (processData [1..5])
    putStrLn $ "Sum: " ++ show sum
    putStrLn "Logs:"
    mapM_ putStrLn logs
    
    -- Do notation
    putStrLn "\n--- Do Notation ---"
    print example1
    print example2
    print $ evalState example3 0
    
    putStrLn "\n=== Monads Demo Complete ==="

