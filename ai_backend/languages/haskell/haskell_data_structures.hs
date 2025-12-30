
-- Haskell Data Structures - Advanced Data Types and Algorithms
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Binary Search Tree
data BST a = Leaf | Node (BST a) a (BST a) deriving (Show, Functor, Foldable, Traversable)

insertBST :: (Ord a) => a -> BST a -> BST a
insertBST x Leaf = Node Leaf x Leaf
insertBST x (Node left val right)
    | x < val   = Node (insertBST x left) val right
    | x > val   = Node left val (insertBST x right)
    | otherwise = Node left val right  -- No duplicates

searchBST :: (Ord a) => a -> BST a -> Bool
searchBST _ Leaf = False
searchBST x (Node left val right)
    | x == val  = True
    | x < val   = searchBST x left
    | otherwise = searchBST x right

deleteBST :: (Ord a) => a -> BST a -> BST a
deleteBST _ Leaf = Leaf
deleteBST x (Node left val right)
    | x < val   = Node (deleteBST x left) val right
    | x > val   = Node left val (deleteBST x right)
    | otherwise = case right of
                    Leaf -> left
                    Node _ rVal _ -> Node left rVal (deleteBSTMin right)
    where deleteBSTMin (Node Leaf val _) = val
          deleteBSTMin (Node left _ _)   = deleteBSTMin left

inorder :: BST a -> [a]
inorder Leaf = []
inorder (Node left val right) = inorder left ++ [val] ++ inorder right

preorder :: BST a -> [a]
preorder Leaf = []
preorder (Node left val right) = [val] ++ preorder left ++ preorder right

postorder :: BST a -> [a]
postorder Leaf = []
postorder (Node left val right) = postorder left ++ postorder right ++ [val]

-- | AVL Tree (Self-balancing BST)
data AVL a = AVLLeaf | AVLNode (AVL a) a (AVL a) Int deriving (Show)

height :: AVL a -> Int
height AVLLeaf = 0
height (AVLNode _ _ _ h) = h

balanceFactor :: AVL a -> Int
balanceFactor AVLLeaf = 0
balanceFactor (AVLNode left _ right _) = height left - height right

rotateLeft :: AVL a -> AVL a
rotateLeft (AVLNode a x (AVLNode b y c h)) = AVLNode (AVLNode a x b h) y c (max (height a) (height b) + 1)

rotateRight :: AVL a -> AVL a
rotateRight (AVLNode (AVLNode a x b h) y c _) = AVLNode a x (AVLNode b y c h) (max (height a) (height b) + 1)

balanceAVL :: AVL a -> AVL a
balanceAVL node@(AVLNode left val right h) =
    case (balanceFactor left, balanceFactor right) of
        (2, 0) -> rotateRight (AVLNode left val right h)
        (-2, 0) -> rotateLeft (AVLNode left val right h)
        (2, 1) -> rotateRight (AVLNode left val right h)
        (-2, -1) -> rotateLeft (AVLNode left val right h)
        _ -> node

insertAVL :: (Ord a) => a -> AVL a -> AVL a
insertAVL x AVLLeaf = AVLNode AVLLeaf x AVLLeaf 1
insertAVL x (AVLNode left val right h)
    | x < val   = balanceAVL $ AVLNode (insertAVL x left) val right (max (height left) (height right) + 1)
    | x > val   = balanceAVL $ AVLNode left val (insertAVL x right) (max (height left) (height right) + 1)
    | otherwise = AVLNode left val right h

-- | Red-Black Tree
data Color = Red | Black deriving (Show, Eq)

data RBTree a = RBLeaf | RBNode Color a (RBTree a) (RBTree a) deriving (Show)

balanceRB :: RBTree a -> RBTree a
balanceRB (RBNode Black z (RBNode Red y (RBNode Red x a b) c) d) =
    RBNode Red y (RBNode Black x a b) (RBNode Black z c d)
balanceRB (RBNode Black z (RBNode Red x a (RBNode Red y b c)) d) =
    RBNode Red y (RBNode Black x a b) (RBNode Black z c d)
balanceRB (RBNode Black x a (RBNode Red y b (RBNode Red z c d))) =
    RBNode Red y (RBNode Black x a b) (RBNode Black z c d)
balanceRB (RBNode Black x a (RBNode Red z (RBNode Red y b c) d)) =
    RBNode Red y (RBNode Black x a b) (RBNode Black z c d)
balanceRB tree = tree

insertRB :: (Ord a) => a -> RBTree a -> RBTree a
insertRB x tree = 
    let inserted = insert tree
        RBNode _ z left right = inserted
    in RBNode Black z left right
    where
        insert RBLeaf = RBNode Red x RBLeaf RBLeaf
        insert (RBNode c y left right)
            | x < y     = balanceRB $ RBNode c y (insert left) right
            | x > y     = balanceRB $ RBNode c y left (insert right)
            | otherwise = RBNode c y left right

-- | Trie (Prefix Tree)
data Trie a = TrieNode (Maybe a) [(Char, Trie a)] deriving (Show)

emptyTrie :: Trie a
emptyTrie = TrieNode Nothing []

insertTrie :: String -> a -> Trie a -> Trie a
insertTrie [] val (TrieNode _ children) = TrieNode (Just val) children
insertTrie (c:cs) val (TrieNode mVal children) =
    let child = case lookup c children of
                  Just t -> t
                  Nothing -> emptyTrie
        newChild = insertTrie cs val child
    in TrieNode mVal ((c, newChild) : filter (\(c', _) -> c' /= c) children)

searchTrie :: String -> Trie a -> Maybe a
searchTrie [] (TrieNode mVal _) = mVal
searchTrie (c:cs) (TrieNode _ children) = 
    case lookup c children of
        Just t -> searchTrie cs t
        Nothing -> Nothing

startsWith :: String -> Trie a -> [String]
startsWith prefix trie = 
    let node = go prefix trie
    in case node of
         Just t -> allWords prefix t
         Nothing -> []
    where
        go [] t = Just t
        go (c:cs) (TrieNode _ children) = 
            case lookup c children of
                Just t -> go cs t
                Nothing -> Nothing
        allWords prefix (TrieNode Nothing []) = []
        allWords prefix (TrieNode (Just val) []) = [prefix]
        allWords prefix (TrieNode mVal children) =
            let withVal = case mVal of
                           Just _ -> [prefix]
                           Nothing -> []
                fromChildren = concatMap (\(c, t) -> allWords (prefix ++ [c]) t) children
            in withVal ++ fromChildren

-- | Heap (Priority Queue)
data Heap a = HeapEmpty | HeapNode Int a (Heap a) (Heap a) deriving (Show)

heapSize :: Heap a -> Int
heapSize HeapEmpty = 0
heapSize (HeapNode n _ _ _) = n

heapSwap :: (Ord a) => Heap a -> Heap a
heapSwap (HeapNode n x left (HeapNode m y right leftRight)) =
    if n < m then HeapNode n x left (HeapNode m y right leftRight)
    else HeapNode m y (HeapNode n x left right) leftRight

insertHeap :: (Ord a) => a -> Heap a -> Heap a
insertHeap x HeapEmpty = HeapNode 1 x HeapEmpty HeapEmpty
insertHeap x (HeapNode n y left right)
    | x < y     = HeapNode (n + 1) x (heapSwap (HeapNode n y left right)) HeapEmpty
    | otherwise = HeapNode (n + 1) y left (insertHeap x right)

extractMin :: (Ord a) => Heap a -> Maybe (a, Heap a)
extractMin HeapEmpty = Nothing
extractMin (HeapNode _ x left right) = Just (x, mergeHeaps left right)

mergeHeaps :: (Ord a) => Heap a -> Heap a -> Heap a
mergeHeaps HeapEmpty h = h
mergeHeaps h HeapEmpty = h
mergeHeaps h1@(HeapNode n1 x l1 r1) h2@(HeapNode n2 y l2 r2)
    | x < y     = HeapNode (n1 + n2) x l1 (mergeHeaps r1 h2)
    | otherwise = HeapNode (n1 + n2) y l2 (mergeHeaps h1 r2)

-- | Graph (Adjacency List)
data Graph a = Graph [(a, [a])] deriving (Show)

addVertex :: (Eq a) => a -> Graph a -> Graph a
addVertex v (Graph edges) 
    | v `elem` map fst edges = Graph edges
    | otherwise = Graph ((v, []) : edges)

addEdge :: (Eq a) => a -> a -> Graph a -> Graph a
addEdge from to (Graph edges) = 
    Graph $ map (\(v, adj) -> 
        if v == from then (v, to : adj)
        else if v == to then (from : adj)
        else (v, adj)
    ) edges

bfs :: (Eq a) => a -> Graph a -> [a]
bfs start (Graph edges) = 
    let adjList = Map.fromList edges
        go visited queue
            | null queue = reverse visited
            | otherwise  = let (v:vs) = queue
                           in if v `elem` visited 
                              then go visited vs
                              else go (v:visited) (vs ++ Map.findWithDefault [] v adjList)
    in go [] [start]

dfs :: (Eq a) => a -> Graph a -> [a]
dfs start (Graph edges) = 
    let adjList = Map.fromList edges
        go visited [] = reverse visited
        go visited (v:vs)
            | v `elem` visited = go visited vs
            | otherwise        = go (v:visited) (Map.findWithDefault [] v adjList ++ vs)
    in go [] [start]

dijkstra :: (Eq a, Ord b, Num b) => a -> a -> Graph a -> Maybe b
dijkstra start end (Graph edges) = undefined

-- | Disjoint Set (Union-Find)
data DisjointSet a = DSU (Map.Map a a) (Map.Map a Int) deriving (Show)

makeSet :: (Eq a, Hashable a) => a -> DisjointSet a
makeSet x = DSU (Map.singleton x x) (Map.singleton x 1)

find :: (Eq a, Hashable a) => a -> DisjointSet a -> a
find x (DSU parent _) = 
    case Map.lookup x parent of
        Just x' | x' /= x -> find x' (DSU parent (Map.singleton x x'))
        _ -> x

union :: (Eq a, Hashable a) => a -> a -> DisjointSet a -> DisjointSet a
union x y dsu@(DSU parent rank) = 
    let rx = find x dsu
        ry = find y dsu
    in if rx == ry then dsu
       else case (Map.lookup rx rank, Map.lookup ry rank) of
              (Just rxRank, Just ryRank)
                  | rxRank < ryRank -> DSU (Map.insert rx ry parent) (Map.insert ry (ryRank + rxRank) rank)
                  | rxRank > ryRank -> DSU (Map.insert ry rx parent) (Map.insert rx (rxRank + ryRank) rank)
              _ -> dsu

-- | Queue (FIFO)
data Queue a = Queue [a] [a] deriving (Show)

emptyQueue :: Queue a
emptyQueue = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue inList outList) = Queue (x : inList) outList

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue inList (x:outList)) = Just (x, Queue inList outList)
dequeue (Queue inList []) = 
    case reverse inList of
        (x:outList) -> Just (x, Queue [] outList)
        [] -> Nothing

-- | Stack (LIFO)
data Stack a = Stack [a] deriving (Show)

emptyStack :: Stack a
emptyStack = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x

-- | Hash Table
data HashTable k v = HashTable 
    { size :: Int
    , buckets :: [ [(k, v)] ]
    } deriving (Show)

defaultSize :: Int
defaultSize = 16

hashKey :: (Hashable k) => Int -> k -> Int
hashKey size key = rem (hash key) size

emptyHashTable :: HashTable k v
emptyHashTable = HashTable defaultSize (replicate defaultSize [])

insertHT :: (Eq k, Hashable k) => k -> v -> HashTable k v -> HashTable k v
insertHT key value (HashTable size buckets) = 
    let idx = hashKey size key
        newBuckets = insertAtIndex idx newPair (buckets !! idx) buckets
    in HashTable size newBuckets
    where
        newPair = (key, value)
        insertAtIndex i pair [] acc = acc ++ [ [pair] ]
        insertAtIndex i pair (p:ps) (b:bs)
            | i == 0    = (pair : b) : bs
            | otherwise = b : insertAtIndex (i - 1) pair ps bs

lookupHT :: (Eq k, Hashable k) => k -> HashTable k v -> Maybe v
lookupHT key (HashTable size buckets) = 
    let idx = hashKey size key
        bucket = buckets !! idx
    in lookup key bucket

deleteHT :: (Eq k, Hashable k) => k -> HashTable k v -> HashTable k v
deleteHT key (HashTable size buckets) = 
    let idx = hashKey size key
        newBuckets = deleteFromIndex idx key (buckets !! idx) buckets
    in HashTable size newBuckets
    where
        deleteFromIndex i _ [] acc = acc
        deleteFromIndex i k (p:ps) (b:bs)
            | i == 0    = ps : bs
            | otherwise = b : deleteFromIndex (i - 1) k ps bs

-- | LRU Cache
data LRUCache k v = LRUCache 
    { capacity :: Int
    , cache :: Map.Map k (v, Int)  -- value and timestamp
    , counter :: Int
    } deriving (Show)

emptyLRU :: Int -> LRUCache k v
emptyLRU cap = LRUCache cap Map.empty 0

getLRU :: (Eq k) => k -> LRUCache k v -> (Maybe v, LRUCache k v)
getLRU key cache@(LRUCache cap m counter) = 
    case Map.lookup key m of
        Just (val, _) -> 
            let updatedM = Map.insert key (val, counter + 1) m
            in (Just val, LRUCache cap updatedM (counter + 1))
        Nothing -> (Nothing, cache)

putLRU :: (Eq k) => k -> v -> LRUCache k v -> LRUCache k v
putLRU key val cache@(LRUCache cap m counter) = 
    let newM = Map.insert key (val, counter + 1) m
        trimmedM = if Map.size newM > cap 
                   then Map.deleteMin newM
                   else newM
    in LRUCache cap trimmedM (counter + 1)

-- | Main function
main :: IO ()
main = do
    putStrLn "=== Haskell Data Structures Demo ==="
    
    -- BST Example
    putStrLn "\n--- Binary Search Tree ---"
    let bst = foldr insertBST Leaf [5, 3, 7, 1, 9, 2, 4]
    print $ inorder bst
    print $ searchBST 4 bst
    print $ searchBST 6 bst
    
    -- Trie Example
    putStrLn "\n--- Trie ---"
    let trie = emptyTrie
        trie' = insertTrie "hello" 1 $ insertTrie "world" 2 $ insertTrie "hell" 3 $ insertTrie "help" 4 emptyTrie
    print $ searchTrie "hello" trie'
    print $ startsWith "hel" trie'
    
    -- Heap Example
    putStrLn "\n--- Heap ---"
    let heap = foldr insertHeap HeapEmpty [5, 3, 7, 1, 9, 2, 4]
    print $ extractMin heap
    
    -- Queue Example
    putStrLn "\n--- Queue ---"
    let q = enqueue 1 $ enqueue 2 $ enqueue 3 emptyQueue
    print $ dequeue q
    
    -- Stack Example
    putStrLn "\n--- Stack ---"
    let s = push 1 $ push 2 $ push 3 emptyStack
    print $ pop s
    
    putStrLn "\n=== Data Structures Demo Complete ==="

