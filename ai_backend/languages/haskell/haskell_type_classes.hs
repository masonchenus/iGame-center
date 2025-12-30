
-- Haskell Type Classes - Advanced Type System Concepts
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Basic type class
class Printable a where
    printVal :: a -> String

instance Printable Int where
    printVal x = "Int: " ++ show x

instance Printable Char where
    printVal x = "Char: '" ++ [x] ++ "'"

instance Printable [Char] where
    printVal x = "String: \"" ++ x ++ "\""

-- | Functor type class
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
    fmap' = map

instance Functor' Maybe where
    fmap' _ Nothing = Nothing
    fmap' f (Just x) = Just (f x)

instance Functor' ((,) e) where
    fmap' f (e, x) = (e, f x)

-- | Monad type class
class Monad' m where
    return' :: a -> m a
    bind' :: m a -> (a -> m b) -> m b

instance Monad' [] where
    return' x = [x]
    bind' xs f = concatMap f xs

instance Monad' Maybe where
    return' = Just
    bind' Nothing _ = Nothing
    bind' (Just x) f = f x

-- | Applicative type class
class Applicative' f where
    pure' :: a -> f a
    app' :: f (a -> b) -> f a -> f b

instance Applicative' [] where
    pure' = return'
    app' fs xs = bind' (fmap' (\f -> \x -> f x) fs) (\f -> fmap' f xs)

-- | Monoid type class
class Monoid' a where
    mempty' :: a
    mappend' :: a -> a -> a

instance Monoid' [a] where
    mempty' = []
    mappend' = (++)

instance Monoid' String where
    mempty' = ""
    mappend' = (++)

instance Monoid' Ordering where
    mempty' = EQ
    mappend' x y = if x == EQ then y else x

-- | Semigroup type class
class Semigroup' a where
    (<>') :: a -> a -> a

instance Semigroup' [a] where
    (<>') = (++)

instance Semigroup' Ordering where
    LT <>' _ = LT
    EQ <>' y = y
    GT <>' _ = GT

-- | Foldable type class
class Foldable' t where
    foldr' :: (a -> b -> b) -> b -> t a -> b

instance Foldable' [] where
    foldr' f z [] = z
    foldr' f z (x:xs) = f x (foldr' f z xs)

instance Foldable' Maybe where
    foldr' _ z Nothing = z
    foldr' f z (Just x) = f x z

-- | Traversable type class
class (Functor' t, Foldable' t) => Traversable' t where
    traverse' :: Applicative' f => (a -> f b) -> t a -> f (t b)

instance Traversable' [] where
    traverse' f [] = pure' []
    traverse' f (x:xs) = pure' (:) <*> f x <*> traverse' f xs

instance Traversable' Maybe where
    traverse' _ Nothing = pure' Nothing
    traverse' f (Just x) = pure' Just <*> f x

-- | Show and Read type classes
data Point = Point { x :: Double, y :: Double } deriving (Show, Read)

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

-- | Eq type class with custom instance
data Person = Person { name :: String, age :: Int }

instance Eq Person where
    (Person n1 a1) == (Person n2 a2) = n1 == n2 && a1 == a2

instance Ord Person where
    compare (Person n1 a1) (Person n2 a2) = compare (n1, a1) (n2, a2)

-- | Enum type class
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Enum, Bounded)

instance Eq Weekday where
    Monday == Monday = True
    Tuesday == Tuesday = True
    -- ... etc

-- | Bounded type class
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Show, Enum)

instance Bounded Month where
    minBound = Jan
    maxBound = Dec

-- | Num type class
data Complex = Complex { real :: Double, imag :: Double }

instance Num Complex where
    (Complex r1 i1) + (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)
    (Complex r1 i1) - (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)
    (Complex r1 i1) * (Complex r2 i2) = Complex (r1*r2 - i1*i2) (r1*i2 + i1*r2)
    abs (Complex r i) = Complex (sqrt (r*r + i*i)) 0
    signum (Complex 0 0) = Complex 0 0
    signum (Complex r i) = Complex (r / mag) (i / mag)
        where mag = sqrt (r*r + i*i)
    fromInteger n = Complex (fromInteger n) 0

-- | Fractional type class
instance Fractional Complex where
    (Complex r1 i1) / (Complex r2 i2) = 
        let denom = r2*r2 + i2*i2
        in Complex ((r1*r2 + i1*i2) / denom) ((i1*r2 - r1*i2) / denom)
    fromRational r = Complex (fromRational r) 0

-- | Floating type class
instance Floating Complex where
    pi = Complex pi 0
    exp (Complex r i) = Complex (exp r * cos i) (exp r * sin i)
    log (Complex r i) = Complex (log (sqrt (r*r + i*i))) (atan2 i r)
    sin (Complex r i) = Complex (sin r * cosh i) (cos r * sinh i)
    cos (Complex r i) = Complex (cos r * cosh i) (-sin r * sinh i)
    asin (Complex r i) = undefined
    acos (Complex r i) = undefined
    atan (Complex r i) = undefined
    sinh (Complex r i) = Complex (sinh r * cos i) (cosh r * sin i)
    cosh (Complex r i) = Complex (cosh r * cos i) (sinh r * sin i)
    asinh (Complex r i) = undefined
    acosh (Complex r i) = undefined
    atanh (Complex r i) = undefined

-- | Multi-parameter type classes
class Convertible a b where
    convert :: a -> b

instance Convertible Int Double where
    convert = fromIntegral

instance Convertible Double Int where
    convert = round

instance Convertible Bool Int where
    convert False = 0
    convert True = 1

instance Convertible Int Bool where
    convert 0 = False
    convert _ = True

-- | Functional dependencies
class Collection c e | c -> e where
    empty :: c
    insert :: e -> c -> c
    member :: e -> c -> Bool

instance Collection [Int] Int where
    empty = []
    insert x xs = x:xs
    member x xs = x `elem` xs

instance Collection (Maybe a) a where
    empty = Nothing
    insert x Nothing = Just x
    insert x (Just _) = Just x  -- No duplicates for simplicity
    member x (Just y) = x == y
    member _ Nothing = False

-- | GADTs (Generalized Algebraic Data Types)
data Expr a where
    LitInt :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq :: Eq a => Expr a -> Expr a -> Expr Bool
    If :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LitInt n) = n
evalExpr (LitBool b) = b
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Eq e1 e2) = evalExpr e1 == evalExpr e2
evalExpr (If cond e1 e2) = if evalExpr cond then evalExpr e1 else evalExpr e2

-- | Phantom types
data Proxy a = Proxy

class ToJSON a where
    toJSON :: a -> String

instance ToJSON Int where
    toJSON n = show n

instance ToJSON String where
    toJSON s = "\"" ++ s ++ "\""

instance ToJSON a => ToJSON [a] where
    toJSON xs = "[" ++ intercalate ", " (map toJSON xs) ++ "]"

-- | Type families
type family Elem c where
    Elem [] = ()
    Elem (x:xs) = x

-- | Default methods
class Default a where
    def :: a

instance Default Int where def = 0
instance Default Double where def = 0.0
instance Default Bool where def = False
instance Default [a] where def = []
instance Default (Maybe a) where def = Nothing

-- | Deriving strategies
newtype MyList a = MyList { getList :: [a] }
    deriving (Eq, Functor, Foldable) via []

-- | Overloaded labels
data Person = Person { _name :: String, _age :: Int }

-- | Type classes with defaults
class Monoid'' a => Group'' a where
    invert :: a -> a
    invert x = mempty'' `mappend''` x

instance Group'' Int where
    invert = negate

-- | main function
main :: IO ()
main = do
    putStrLn "=== Haskell Type Classes Demo ==="
    
    -- Printable instances
    putStrLn "\n--- Printable Type Class ---"
    putStrLn $ printVal (42 :: Int)
    putStrLn $ printVal 'A'
    putStrLn $ printVal "Hello"
    
    -- Complex numbers
    putStrLn "\n--- Complex Numbers ---"
    let c1 = Complex 3 4
        c2 = Complex 1 2
    putStrLn $ "c1 = " ++ show c1
    putStrLn $ "c2 = " ++ show c2
    putStrLn $ "c1 + c2 = " ++ show (c1 + c2)
    putStrLn $ "c1 * c2 = " ++ show (c1 * c2)
    putStrLn $ "1/c1 = " ++ show (recip c1)
    
    -- Convertible
    putStrLn "\n--- Convertible Type Class ---"
    print $ convert (42 :: Int) :: Double
    print $ convert (3.14 :: Double) :: Int
    print $ convert True :: Int
    print $ convert (0 :: Int) :: Bool
    
    -- Expressions
    putStrLn "\n--- GADT Expressions ---"
    let expr = Add (LitInt 5) (Mul (LitInt 3) (LitInt 4))
    putStrLn $ "Expression: 5 + 3 * 4 = " ++ show (evalExpr expr)
    
    -- Default values
    putStrLn "\n--- Default Type Class ---"
    print (def :: Int)
    print (def :: Double)
    print (def :: Bool)
    print (def :: [Int])
    print (def :: Maybe Int)
    
    putStrLn "\n=== Type Classes Demo Complete ==="

