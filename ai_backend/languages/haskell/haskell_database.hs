
-- Haskell Database - Database Operations and SQL-like Functionality
{-# LANGUAGE FlexibleInstances #-}

import Data.List
import Data.Maybe
import Data.Char

-- | Simple database table
data Table a = Table
    { tableName :: String
    , columns :: [String]
    , rows :: [[a]]
    } deriving (Show)

-- | Column operations
data ColumnOp = Asc | Desc deriving (Show)

-- | Select query
data SelectQuery = Select
    { selectFrom :: String
    , selectWhere :: Maybe (Row -> Bool)
    , selectOrderBy :: Maybe (String, ColumnOp)
    , selectLimit :: Maybe Int
    , selectOffset :: Maybe Int
    , selectColumns :: Maybe [String]
    } deriving (Show)

type Row = [(String, String)]

-- | Create table
createTable :: String -> [String] -> Table String
createTable name cols = Table name cols []

-- | Insert row
insertRow :: Table String -> [(String, String)] -> Table String
insertRow table row = 
    table { rows = row : rows table }

-- | Simple SQL parser
data SQLStatement = 
    SelectStmt SelectQuery
    | InsertStmt String [(String, String)]
    | UpdateStmt String [(String, String)] (Row -> Bool)
    | DeleteStmt String (Row -> Bool)
    | CreateTableStmt String [String]
    | DropTableStmt String
    deriving (Show)

parseSQL :: String -> Maybe SQLStatement
parseSQL sql = 
    let words = words sql
    in case map toLower (head words) of
        "select" -> Just (SelectStmt (parseSelect words))
        "insert" -> Just (InsertStmt "table" [])
        "update" -> Just (UpdateStmt "table" [] (const True))
        "delete" -> Just (DeleteStmt "table" (const True))
        "create" -> Just (CreateTableStmt "table" [])
        "drop" -> Just (DropTableStmt "table")
        _ -> Nothing

parseSelect :: [String] -> SelectQuery
parseSelect words = Select
    { selectFrom = "table"  -- Simplified
    , selectWhere = Nothing
    , selectOrderBy = Nothing
    , selectLimit = Nothing
    , selectOffset = Nothing
    , selectColumns = Nothing
    }

-- | Execute query on table
executeQuery :: Table String -> SelectQuery -> [[(String, String)]]
executeQuery table query = 
    let allRows = rows table
        filtered = case selectWhere query of
                    Nothing -> allRows
                    Just pred -> filter pred allRows
        sorted = case selectOrderBy query of
                  Nothing -> filtered
                  Just (col, order) -> 
                    let sortFn = case order of
                                  Asc -> compare
                                  Desc -> flip compare
                    in sortBy (\r1 r2 -> 
                        let v1 = fromMaybe "" (lookup col r1)
                            v2 = fromMaybe "" (lookup col r2)
                        in sortFn v1 v2) filtered
        limited = case selectLimit query of
                  Nothing -> sorted
                  Just n -> take n sorted
        offset = fromMaybe 0 (selectOffset query)
    in drop offset limited

-- | Join operations
data JoinType = InnerJoin | LeftJoin | RightJoin | OuterJoin
    deriving (Show)

join :: JoinType -> Table String -> Table String -> (String, String) -> [[(String, String)]]
join joinType table1 table2 (col1, col2) = undefined

-- | Aggregation functions
data Aggregation = Count | Sum String | Avg String | Min String | Max String | CountDistinct String
    deriving (Show)

aggregate :: Aggregation -> [[(String, String)]] -> String
aggregate Count rows = show (length rows)
aggregate (Sum col) rows = show (sum [ read (fromMaybe "0" (lookup r col)) | r <- rows ])
aggregate (Avg col) rows = show (average [ read (fromMaybe "0" (lookup r col)) | r <- rows ])
    where average xs = sum xs / fromIntegral (length xs)
aggregate (Min col) rows = minimum [ fromMaybe "" (lookup r col) | r <- rows ]
aggregate (Max col) rows = maximum [ fromMaybe "" (lookup r col) | r <- rows ]
aggregate (CountDistinct col) rows = show (length (nub [ fromMaybe "" (lookup r col) | r <- rows ]))

-- | Group by
groupBy :: [String] -> [[(String, String)]] -> [[(String, String)]]
groupBy keys rows = 
    let groups = groupByKey keys rows
    in map head (Map.elems groups)
    where
        groupByKey [] rows = Map.fromList [ ([], rows) ]
        groupByKey (k:ks) rows = 
            Map.fromListWith (++) [ (map (\r -> fromMaybe "" (lookup r k)) row, [row]) | row <- rows ]

-- | Index management
data Index = Index
    { indexName :: String
    , indexTable :: String
    , indexColumn :: String
    , indexData :: Map.Map String [Int]
    } deriving (Show)

createIndex :: String -> String -> Table String -> Index
createIndex name col table = 
    let indexData = Map.fromList [ (value, [i | (i, row) <- zip [0..] (rows table), lookup col row == Just value]) 
                                 | row <- rows table
                                 , value <- [fromMaybe "" (lookup col row)] ]
    in Index name (tableName table) col indexData

lookupIndex :: Index -> String -> Maybe [Int]
lookupIndex index value = Map.lookup value (indexData index)

-- | Transaction management
data Transaction = Transaction
    { transId :: Int
    , transTables :: [(Table String, Table String)]  -- (original, modified)
    , transStatus :: TransactionStatus
    } deriving (Show)

data TransactionStatus = Active | Committed | RolledBack
    deriving (Show)

beginTransaction :: Int -> Transaction
beginTransaction tid = Transaction tid [] Active

updateInTransaction :: Transaction -> String -> [(String, String)] -> Transaction
updateInTransaction trans tableName newRow = 
    trans { transTables = (originalTable, newRow) : transTables trans }
    where originalTable = undefined

commitTransaction :: Transaction -> [Table String]
commitTransaction trans = undefined

rollbackTransaction :: Transaction -> [Table String]
rollbackTransaction trans = undefined

-- | Database connection (mock)
data DatabaseConnection = DatabaseConnection
    { connHost :: String
    , connPort :: Int
    , connDatabase :: String
    , connTables :: [Table String]
    } deriving (Show)

connect :: String -> Int -> String -> DatabaseConnection
connect host port database = 
    DatabaseConnection host port database []

disconnect :: DatabaseConnection -> ()
disconnect _ = ()

execute :: DatabaseConnection -> SQLStatement -> (DatabaseConnection, [[(String, String)]])
execute conn (SelectStmt query) = 
    let result = executeQuery (head (connTables conn)) query
    in (conn, result)
execute conn _ = (conn, [])

-- | Simple query optimizer
optimizeQuery :: SelectQuery -> SelectQuery
optimizeQuery query = 
    query { selectWhere = optimizePredicate (selectWhere query) }

optimizePredicate :: Maybe (Row -> Bool) -> Maybe (Row -> Bool)
optimizePredicate Nothing = Nothing
optimizePredicate (Just pred) = Just pred

-- | Query planner
data QueryPlan = SeqScan String | IndexScan String String | JoinPlan QueryPlan QueryPlan
    deriving (Show)

planQuery :: SelectQuery -> Table String -> QueryPlan
planQuery query table = 
    case selectWhere query of
        Nothing -> SeqScan (tableName table)
        Just _ -> IndexScan (tableName table) "index"

-- | Result set pagination
paginate :: [[(String, String)]] -> Int -> Int -> [[(String, String)]]
paginate rows pageSize pageNum = 
    let start = (pageNum - 1) * pageSize
    in take pageSize (drop start rows)

-- | Result metadata
data ResultMetadata = ResultMetadata
    { columnNames :: [String]
    , columnTypes :: [String]
    , rowCount :: Int
    } deriving (Show)

getMetadata :: [[(String, String)]] -> ResultMetadata
getMetadata rows = 
    let cols = if null rows then [] else map fst (head rows)
        types = map (const "STRING") cols
    in ResultMetadata cols types (length rows)

-- | Error handling
data DatabaseError = 
    ConnectionError String
    | QueryError String
    | ConstraintViolation String
    | DuplicateKey String
    deriving (Show, Eq)

-- | main function
main :: IO ()
main = do
    putStrLn "=== Haskell Database Demo ==="
    
    -- Create table
    putStrLn "\n--- Table Operations ---"
    let usersTable = createTable "users" ["id", "name", "email", "age"]
    putStrLn $ "Created table: " ++ tableName usersTable
    
    -- Insert rows
    let user1 = [("id", "1"), ("name", "Alice"), ("email", "alice@example.com"), ("age", "30")]
    let user2 = [("id", "2"), ("name", "Bob"), ("email", "bob@example.com"), ("age", "25")]
    let user3 = [("id", "3"), ("name", "Charlie"), ("email", "charlie@example.com"), ("age", "35")]
    
    let usersTable' = foldl insertRow usersTable [user1, user2, user3]
    putStrLn $ "Inserted " ++ show (length (rows usersTable')) ++ " rows"
    
    -- Execute query
    putStrLn "\n--- Query Execution ---"
    let query = Select "users" Nothing Nothing (Just 2) Nothing Nothing
    let results = executeQuery usersTable' query
    print (length results) ++ " results"
    
    -- Aggregation
    putStrLn "\n--- Aggregation ---"
    putStrLn $ "Count: " ++ aggregate Count (rows usersTable')
    putStrLn $ "Max age: " ++ aggregate (Max "age") (rows usersTable')
    
    -- SQL parsing
    putStrLn "\n--- SQL Parsing ---"
    let sql = "SELECT * FROM users WHERE age > 25"
    case parseSQL sql of
        Just stmt -> putStrLn $ "Parsed: " ++ show stmt
        Nothing -> putStrLn "Parse error"
    
    -- Pagination
    putStrLn "\n--- Pagination ---"
    let page1 = paginate (rows usersTable') 2 1
    print page1
    
    -- Metadata
    putStrLn "\n--- Result Metadata ---"
    let metadata = getMetadata (rows usersTable')
    putStrLn $ "Columns: " ++ show (columnNames metadata)
    putStrLn $ "Rows: " ++ show (rowCount metadata)
    
    putStrLn "\n=== Database Demo Complete ==="

