
-- Haskell JSON - JSON Parsing and Generation
{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad

-- | JSON data types
data JSON = JNull 
          | JBool Bool 
          | JNumber Double 
          | JString String 
          | JArray [JSON] 
          | JObject [(String, JSON)]
    deriving (Show, Eq)

-- | JSON parser
class FromJSON a where
    parseJSON :: JSON -> Maybe a

class ToJSON a where
    toJSON :: a -> JSON

-- | JSON parser implementation
parseJSONString :: String -> Maybe JSON
parseJSONString "" = Nothing
parseJSONString ('"':rest) = 
    case break (== '"') rest of
        (str, '"':remaining) -> 
            if all escaped str
            then Just (JString (unescape str))
            else Nothing
        _ -> Nothing
    where
        escaped [] = True
        escaped ('\\':_) = True
        escaped (_:cs) = escaped cs
        unescape = id  -- Simplified

parseJSONNumber :: String -> Maybe JSON
parseJSONNumber str = 
    if all isDigitOrDotOrMinus str && not (null str)
    then Just (JNumber (read str))
    else Nothing

isDigitOrDotOrMinus :: Char -> Bool
isDigitOrDotOrMinus c = isDigit c || c == '.' || c == '-'

parseJSONBool :: String -> Maybe JSON
parseJSONBool "true" = Just (JBool True)
parseJSONBool "false" = Just (JBool False)
parseJSONBool _ = Nothing

parseJSONNull :: String -> Maybe JSON
parseJSONNull "null" = Just JNull
parseJSONNull _ = Nothing

-- | Parse JSON value
parseJSONValue :: String -> Maybe (JSON, String)
parseJSONValue [] = Nothing
parseJSONValue (' ':rest) = parseJSONValue rest
parseJSONValue ('{':rest) = parseJSONObject rest
parseJSONValue ('[':rest) = parseJSONArray rest
parseJSONValue str@(c:_) 
    | c == '"' = do
        (JString strVal, remaining) <- parseJSONValue str
        return (JString strVal, remaining)
    | isDigit c || c == '-' = parseJSONNumberValue str
    | isAlpha c = parseJSONKeyword str
    | otherwise = Nothing

parseJSONNumberValue :: String -> Maybe (JSON, String)
parseJSONNumberValue str = 
    let (numStr, rest) = span isDigitOrDotOrMinusOrE str
    in case parseJSONNumber numStr of
        Just json -> return (json, rest)
        Nothing -> Nothing

isDigitOrDotOrMinusOrE :: Char -> Bool
isDigitOrDotOrMinusOrE c = isDigit c || c == '.' || c == '-' || c == 'e' || c == 'E'

parseJSONKeyword :: String -> Maybe (JSON, String)
parseJSONKeyword str = 
    let (keyword, rest) = span isAlpha str
    in case keyword of
        "null" -> return (JNull, rest)
        "true" -> return (JBool True, rest)
        "false" -> return (JBool False, rest)
        _ -> Nothing

-- | Parse JSON object
parseJSONObject :: String -> Maybe (JSON, String)
parseJSONObject ('{':rest) = do
    (members, '}' : remaining) <- parseJSONMembers rest
    return (JObject members, remaining)
parseJSONObject _ = Nothing

parseJSONMembers :: String -> Maybe ([(String, JSON)], String)
parseJSONMembers [] = Nothing
parseJSONMembers str = 
    case parseJSONMember (skipSpaces str) of
        Just ((key, value), remaining) -> do
            more <- parseJSONMembers' remaining
            return ((key, value) : more, remaining')
            where
                parseJSONMembers' s = case skipSpaces s of
                    (',':s') -> parseJSONMembers s'
                    s' -> Just ([], s')
        Nothing -> Just ([], str)

parseJSONMember :: String -> Maybe ((String, JSON), String)
parseJSONMember str = do
    (JString key, ':':remaining) <- parseJSONValue str
    (value, rest) <- parseJSONValue remaining
    return ((key, value), rest)

-- | Parse JSON array
parseJSONArray :: String -> Maybe (JSON, String)
parseJSONArray ('[':rest) = do
    (elements, ']' : remaining) <- parseJSONArrayElements rest
    return (JArray elements, remaining)
parseJSONArray _ = Nothing

parseJSONArrayElements :: String -> Maybe ([JSON], String)
parseJSONElements str = 
    case parseJSONValue (skipSpaces str) of
        Just (value, remaining) -> do
            more <- parseJSONArrayElements' remaining
            return (value : more, remaining')
            where
                parseJSONArrayElements' s = case skipSpaces s of
                    (',':s') -> parseJSONElements s'
                    s' -> Just ([], s')
        Nothing -> Just ([], str)

skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

-- | Generate JSON
class ToJSON a where
    toJSON :: a -> JSON

instance ToJSON Bool where
    toJSON = JBool

instance ToJSON Int where
    toJSON = JNumber . fromIntegral

instance ToJSON Double where
    toJSON = JNumber

instance ToJSON String where
    toJSON = JString

instance ToJSON JSON where
    toJSON = id

instance ToJSON a => ToJSON [a] where
    toJSON = JArray . map toJSON

instance (ToJSON a) => ToJSON (Maybe a) where
    toJSON Nothing = JNull
    toJSON (Just x) = toJSON x

instance (ToJSON a) => ToJSON [(String, a)] where
    toJSON = JObject . map (\(k, v) -> (k, toJSON v))

-- | JSON serialization
jsonToString :: JSON -> String
jsonToString JNull = "null"
jsonToString (JBool True) = "true"
jsonToString (JBool False) = "false"
jsonToString (JNumber n) = show n
jsonToString (JString s) = '"' : escape s ++ "\""
jsonToString (JArray []) = "[]"
jsonToString (JArray (x:xs)) = "[" ++ jsonToString x ++ concat ["," ++ jsonToString y | y <- xs] ++ "]"
jsonToString (JObject []) = "{}"
jsonToString (JObject ((k,v):kvs)) = 
    "{\"" ++ k ++ "\":" ++ jsonToString v ++ 
    concat [",\"" ++ k' ++ "\":" ++ jsonToString v' | (k', v') <- kvs] ++ "}"

escape :: String -> String
escape [] = []
escape ('\\':cs) = '\\' : '\\' : escape cs
escape ('"':cs) = '\\' : '"' : escape cs
escape ('\n':cs) = '\\' : 'n' : escape cs
escape ('\r':cs) = '\\' : 'r' : escape cs
escape ('\t':cs) = '\\' : 't' : escape cs
escape (c:cs) = c : escape cs

-- | Pretty printing JSON
prettyPrint :: Int -> JSON -> String
prettyPrint indent JNull = "null"
prettyPrint indent (JBool True) = "true"
prettyPrint indent (JBool False) = "false"
prettyPrint indent (JNumber n) = show n
prettyPrint indent (JString s) = '"' : s ++ "\""
prettyPrint indent (JArray []) = "[]"
prettyPrint indent (JArray (x:xs)) = 
    "[\n" ++ indentStr ++ prettyPrint (indent + 2) x ++
    concat [",\n" ++ indentStr ++ prettyPrint (indent + 2) y | y <- xs] ++
    "\n" ++ replicate (indent - 2) ' ' ++ "]"
    where indentStr = replicate indent ' '
prettyPrint indent (JObject []) = "{}"
prettyPrint indent (JObject ((k,v):kvs)) = 
    "{\n" ++ indentStr ++ '"' ++ k ++ "\": " ++ prettyPrint (indent + 2) v ++
    concat [",\n" ++ indentStr ++ '"' ++ k' ++ "\": " ++ prettyPrint (indent + 2) v' | (k', v') <- kvs] ++
    "\n" ++ replicate (indent - 2) ' ' ++ "}"
    where indentStr = replicate indent ' '

-- | Minify JSON
minify :: JSON -> String
minify = go
    where
        go JNull = "null"
        go (JBool True) = "true"
        go (JBool False) = "false"
        go (JNumber n) = show n
        go (JString s) = '"' : s ++ "\""
        go (JArray []) = "[]"
        go (JArray (x:xs)) = "[" ++ go x ++ concat ["," ++ go y | y <- xs] ++ "]"
        go (JObject []) = "{}"
        go (JObject ((k,v):kvs)) = 
            "{\"" ++ k ++ "\":" ++ go v ++ 
            concat [",\"" ++ k' ++ "\":" ++ go v' | (k', v') <- kvs] ++ "}"

-- | JSON path navigation
data JSONPath = Key String | Index Int | Wildcard
    deriving (Show)

getJSONPath :: JSON -> [JSONPath] -> Maybe JSON
getJSONPath json [] = Just json
getJSONPath (JObject []) (_:_) = Nothing
getJSONPath (JObject ((k,v):kvs)) (Key key:rest) = 
    if k == key then getJSONPath v rest
    else getJSONPath (JObject kvs) (Key key:rest)
getJSONPath (JArray []) (_:_) = Nothing
getJSONPath (JArray (x:xs)) (Index i:rest) = 
    if i >= 0 && i < length (x:xs)
    then getJSONPath ((x:xs) !! i) rest
    else Nothing
getJSONPath (JArray items) (Wildcard:rest) = 
    Just (JArray [ j | item <- items, j <- [getJSONPath item rest] ])

-- | JSON validation
validateJSON :: String -> Bool
validateJSON str = isJust (parseJSONValue str >>= Just . fst)

-- | JSON schema validation
data JSONSchema = JSONSchema
    { schemaType :: Maybe String
    , schemaRequired :: [String]
    , schemaProperties :: [(String, JSONSchema)]
    , schemaItems :: Maybe JSONSchema
    } deriving (Show)

validateSchema :: JSON -> JSONSchema -> Bool
validateSchema json schema = 
    let typeMatches = case (schemaType schema, json) of
            (Nothing, _) -> True
            (Just "null", JNull) -> True
            (Just "boolean", JBool _) -> True
            (Just "number", JNumber _) -> True
            (Just "string", JString _) -> True
            (Just "array", JArray _) -> True
            (Just "object", JObject _) -> True
            _ -> False
        requiredMatches = case json of
            JObject kvs -> all (`elem` map fst kvs) (schemaRequired schema)
            _ -> null (schemaRequired schema)
    in typeMatches && requiredMatches

-- | Pretty instances
instance Show JSON where
    show = jsonToString

-- | main function
main :: IO ()
main = do
    putStrLn "=== Haskell JSON Demo ==="
    
    -- Parse JSON
    putStrLn "\n--- JSON Parsing ---"
    let jsonStr = "{\"name\": \"Alice\", \"age\": 30, \"active\": true, \"scores\": [1, 2, 3]}"
    print jsonStr
    
    -- Generate JSON
    putStrLn "\n--- JSON Generation ---"
    let person = JObject 
            [ ("name", JString "Bob")
            , ("age", JNumber 25)
            , ("active", JBool True)
            , ("friends", JArray [JString "Alice", JString "Charlie"])
            ]
    putStrLn $ "Generated: " ++ jsonToString person
    
    -- Pretty print
    putStrLn "\n--- Pretty Printing ---"
    putStrLn $ prettyPrint 2 person
    
    -- Minify
    putStrLn "\n--- Minification ---"
    putStrLn $ "Minified: " ++ minify person
    
    -- JSON path
    putStrLn "\n--- JSON Path ---"
    let nested = JObject [("data", JArray [JObject [("value", JNumber 42)]])]
    print $ getJSONPath nested [Key "data", Index 0, Key "value"]
    
    -- Validation
    putStrLn "\n--- Validation ---"
    putStrLn $ "Valid JSON: " ++ show (validateJSON jsonStr)
    
    -- Schema validation
    putStrLn "\n--- Schema Validation ---"
    let schema = JSONSchema 
            { schemaType = Just "object"
            , schemaRequired = ["name", "age"]
            , schemaProperties = []
            , schemaItems = Nothing
            }
    putStrLn $ "Schema valid: " ++ show (validateSchema person schema)
    
    putStrLn "\n=== JSON Demo Complete ==="

