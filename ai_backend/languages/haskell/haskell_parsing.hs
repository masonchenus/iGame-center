
-- Haskell Parsing - Parser Combinators and Text Processing
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List

-- | Basic parser type
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- | Functor instance
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (result, rest) <- p input
        return (f result, rest)

-- | Applicative instance
instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (<*>) (Parser pf) (Parser px) = Parser $ \input -> do
        (f, rest1) <- pf input
        (x, rest2) <- px rest1
        return (f x, rest2)

-- | Alternative instance
instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> 
        case p1 input of
            Nothing -> p2 input
            result -> result

-- | Monad instance
instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> do
        (result, rest) <- p input
        runParser (f result) rest

-- | Basic parsers
item :: Parser Char
item = Parser $ \input ->
    case input of
        (x:xs) -> Just (x, xs)
        [] -> Nothing

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
    char c
    string cs
    return (c:cs)

-- | Character parsers
digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isLetter

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

space :: Parser Space
space = sat isSpace

newline :: Parser Char
newline = char '\n'

tab :: Parser Char
tab = char '\t'

-- | Numeric parsers
integer :: Parser Int
integer = do
    optional (char '-')
    ds <- some digit
    return (read ds)

natural :: Parser Int
natural = do
    ds <- some digit
    return (read ds)

float :: Parser Double
float = do
    optional (char '-')
    intPart <- some digit
    optional (char '.')
    fracPart <- some digit
    return (read (intPart ++ "." ++ fracPart))

-- | String parsers
word :: Parser String
word = some letter

alphaNumString :: Parser String
alphaNumString = some alphaNum

quotedString :: Parser String
quotedString = do
    char '"'
    content <- many (sat (/= '"'))
    char '"'
    return content

-- | Repetition
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = do
    x <- p
    xs <- many (do { sep; p })
    return (x:xs)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
    x <- p
    xs <- some (do { sep; p })
    return (x:xs)

-- | Optional and many
option :: a -> Parser a -> Parser a
option def p = p <|> return def

many :: Parser a -> Parser [a]
many p = some p <|> return []

some :: Parser a -> Parser [a]
some p = do
    x <- p
    xs <- many p
    return (x:xs)

-- | Position tracking
data ParserState = ParserState
    { input :: String
    , position :: Int
    , line :: Int
    , column :: Int
    } deriving (Show)

initialState :: String -> ParserState
initialState input = ParserState input 0 1 1

updatePosition :: ParserState -> Char -> ParserState
updatePosition state '\n' = state { position = position state + 1, line = line state + 1, column = 1 }
updatePosition state c = state { position = position state + 1, column = column state + 1 }

-- | Token parsers
token :: Parser a -> Parser a
token p = do
    many space
    x <- p
    many space
    return x

symbol :: String -> Parser String
symbol s = token (string s)

parens :: Parser a -> Parser a
parens p = do
    symbol "("
    x <- p
    symbol ")"
    return x

brackets :: Parser a -> Parser a
brackets p = do
    symbol "["
    x <- p
    symbol "]"
    return x

braces :: Parser a -> Parser a
braces p = do
    symbol "{"
    x <- p
    symbol "}"
    return x

-- | CSV parser
data CSV = CSV [[String]] deriving (Show)

parseCSV :: String -> CSV
parseCSV input = 
    case runParser csvParser input of
        Just (csv, "") -> csv
        _ -> CSV []
    where
        csvParser = do
            rows <- sepBy row (char '\n')
            return (CSV rows)
        row = sepBy cell (char ',')
        cell = do
            content <- many (sat (\c -> c /= ',' && c /= '\n'))
            return content

-- | JSON parser (simplified)
data JSON = JNull | JBool Bool | JNumber Double | JString String | JArray [JSON] | JObject [(String, JSON)] deriving (Show)

parseJSON :: String -> Maybe JSON
parseJSON input = fst <$> runParser jsonParser input
    where
        jsonParser = do
            spaces
            jvalue
        spaces = many (sat isSpace)
        jvalue = jnull <|> jbool <|> jnumber <|> jstring <|> jarray <|> jobject
        jnull = string "null" >> return JNull
        jbool = (string "true" >> return (JBool True)) <|> (string "false" >> return (JBool False))
        jnumber = fmap JNumber float
        jstring = fmap JString quotedString
        jarray = do
            symbol "["
            values <- sepBy jvalue (symbol ",")
            symbol "]"
            return (JArray values)
        jobject = do
            symbol "{"
            pairs <- sepBy pair (symbol ",")
            symbol "}"
            return (JObject pairs)
        pair = do
            key <- quotedString
            symbol ":"
            value <- jvalue
            return (key, value)

-- | Expression parser
data Expr = Num Int | Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Div Expr Expr deriving (Show)

expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = number <|> parens expr

number :: Parser Expr
number = fmap Num integer

addOp :: Parser (Expr -> Expr -> Expr)
addOp = (symbol "+" >> return Add) <|> (symbol "-" >> return Sub)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = (symbol "*" >> return Mul) <|> (symbol "/" >> return Div)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
    x <- p
    rest x
    where
        rest x = do
            f <- op
            y <- p
            rest (f x y)
            <|> return x

-- | Parsing parentheses
data ParensExpr = PEmpty | PNum Int | PParens ParensExpr deriving (Show)

parensExpr :: Parser ParensExpr
parensExpr = PEmpty <$ symbol "" <|> parens parensExpr <|> fmap PNum number

-- | Regex-like patterns
many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many p
    return (x:xs)

count :: Int -> Parser a -> Parser [a]
count n p = sequence (replicate n p)

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
    open
    x <- p
    close
    return x

-- | Error handling
parseError :: String -> Parser a
parseError msg = Parser $ \_ -> error msg

expected :: String -> Parser a
expected msg = Parser $ \input -> 
    error $ "Expected " ++ msg ++ " at: " ++ input

-- | Pretty printing
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"
showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ " / " ++ showExpr e2 ++ ")"

-- | main function
main :: IO ()
main = do
    putStrLn "=== Haskell Parsing Demo ==="
    
    -- Character parsing
    putStrLn "\n--- Character Parsing ---"
    print $ runParser (char 'a') "abc"
    print $ runParser digit "123abc"
    
    -- String parsing
    putStrLn "\n--- String Parsing ---"
    print $ runParser (string "hello") "hello world"
    print $ runParser quotedString "\"Hello, World!\""
    
    -- Number parsing
    putStrLn "\n--- Number Parsing ---"
    print $ runParser integer "42abc"
    print $ runParser float "3.14159"
    
    -- CSV parsing
    putStrLn "\n--- CSV Parsing ---"
    let csvContent = "Name,Age,City\nAlice,30,NYC\nBob,25,LA"
    print $ parseCSV csvContent
    
    -- JSON parsing
    putStrLn "\n--- JSON Parsing ---"
    let jsonContent = "{\"name\": \"Alice\", \"age\": 30, \"active\": true}"
    print $ parseJSON jsonContent
    
    -- Expression parsing
    putStrLn "\n--- Expression Parsing ---"
    let exprStr = "(1 + 2) * 3"
    case runParser expr exprStr of
        Just (expr, "") -> putStrLn $ "Parsed: " ++ showExpr expr
        _ -> putStrLn "Parse error"
    
    -- Word parsing
    putStrLn "\n--- Word Parsing ---"
    print $ runParser word "hello world"
    
    putStrLn "\n=== Parsing Demo Complete ==="

