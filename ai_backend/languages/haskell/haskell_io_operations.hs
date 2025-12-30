
-- Haskell IO Operations - File Handling and System Interactions
{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import System.Directory
import System.FilePath
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Read entire file into string
readFile' :: FilePath -> IO String
readFile' path = do
    content <- readFile path
    return content

-- | Write string to file
writeFile' :: FilePath -> String -> IO ()
writeFile' path content = do
    writeFile path content

-- | Append to file
appendFile' :: FilePath -> String -> IO ()
appendFile' path content = do
    appendFile path content

-- | Read file line by line
readFileLines :: FilePath -> IO [String]
readFileLines path = do
    content <- readFile path
    return (lines content)

-- | Write lines to file
writeFileLines :: FilePath -> [String] -> IO ()
writeFileLines path lines = do
    writeFile path (unlines lines)

-- | Read file with handle
withFileRead :: FilePath -> (Handle -> IO a) -> IO a
withFileRead path action = withFile path ReadMode action

-- | Write file with handle
withFileWrite :: FilePath -> (Handle -> IO a) -> IO a
withFileWrite path action = withFile path WriteMode action

-- | Read binary file
readBinaryFile :: FilePath -> IO ByteString
readBinaryFile path = BS.readFile path

-- | Write binary file
writeBinaryFile :: FilePath -> ByteString -> IO ()
writeBinaryFile path content = BS.writeFile path content

-- | Read lazy binary file
readLazyBinaryFile :: FilePath -> IO LBS.ByteString
readLazyBinaryFile path = LBS.readFile path

-- | Copy file
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = do
    content <- BS.readFile src
    BS.writeFile dst content

-- | Copy file with handles
copyFile' :: FilePath -> FilePath -> IO ()
copyFile' src dst = do
    withFile src ReadMode $ \srcHandle ->
        withFile dst WriteMode $ \dstHandle ->
            copyHandle srcHandle dstHandle

-- | Copy data between handles
copyHandle :: Handle -> Handle -> IO ()
copyHandle src dst = do
    eof <- hIsEOF src
    unless eof $ do
        chunk <- BS.hGet src 4096
        BS.hPut dst chunk
        copyHandle src dst

-- | Get file size
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(_ :: SomeException) -> return Nothing) $ do
    size <- withFileRead path hFileSize
    return (Just size)

-- | Check if file exists
fileExists :: FilePath -> IO Bool
fileExists path = doesFileExist path

-- | Create directory
createDirectory :: FilePath -> IO ()
createDirectory path = createDirectoryIfMissing False path

-- | Create directory recursively
createDirectoryRecursive :: FilePath -> IO ()
createDirectoryRecursive path = createDirectoryIfMissing True path

-- | List directory contents
listDirectory :: FilePath -> IO [FilePath]
listDirectory path = do
    entries <- getDirectoryContents path
    return $ filter (`notElem` [".", ".."]) entries

-- | Get all files recursively
getAllFiles :: FilePath -> IO [FilePath]
getAllFiles root = do
    isDir <- doesDirectoryExist root
    if isDir
        then do
            entries <- listDirectory root
            concat <$> mapM getAllFiles (map (root </>) entries)
        else return [root]

-- | Get files with extension
getFilesByExtension :: FilePath -> String -> IO [FilePath]
getFilesByExtension path ext = do
    allFiles <- getAllFiles path
    return $ filter (\f -> takeExtension f == ext) allFiles

-- | Remove file
removeFile' :: FilePath -> IO ()
removeFile' path = removeFile path

-- | Remove directory
removeDirectory' :: FilePath -> IO ()
removeDirectory' path = removeDirectory path

-- | Rename file
renameFile' :: FilePath -> FilePath -> IO ()
renameFile' old new = renameDirectory old new

-- | Get current working directory
getCurrentDir :: IO FilePath
getCurrentDir = getCurrentDirectory

-- | Change working directory
changeDirectory :: FilePath -> IO ()
changeDirectory path = setCurrentDirectory path

-- | Get home directory
getHomeDir :: IO FilePath
getHomeDir = getHomeDirectory

-- | Get temporary directory
getTempDir :: IO FilePath
getTempDir = getTemporaryDirectory

-- | Create temporary file
withTempFile :: (FilePath -> IO a) -> IO a
withTempFile action = do
    tempDir <- getTempDir
    let tempFile = tempDir </> "temp_" ++ show (randomNumber 1000000)
    finally (action tempFile) (removeFile tempFile)

-- | Create temporary directory
withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory action = do
    tempDir <- getTempDir
    let tempDirectory = tempDir </> "tempdir_" ++ show (randomNumber 1000000)
    createDirectory tempDirectory
    finally (action tempDirectory) (removeDirectoryRecursive tempDirectory)

-- | Read file safely with exception handling
safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = handle handler (fmap Right (readFile path))
    where
        handler :: IOException -> IO (Either String String)
        handler e = return $ Left (show e)

-- | Write file safely
safeWriteFile :: FilePath -> String -> IO (Either String ())
safeWriteFile path content = handle handler (writeFile path content >> return (Right ()))
    where
        handler :: IOException -> IO (Either String ())
        handler e = return $ Left (show e)

-- | Read lines safely
safeReadFileLines :: FilePath -> IO (Either String [String])
safeReadFileLines path = do
    result <- safeReadFile path
    return $ fmap lines result

-- | File permissions
data FilePermissions = FilePermissions 
    { readable :: Bool
    , writable :: Bool
    , executable :: Bool
    }

getFilePermissions :: FilePath -> IO FilePermissions
getFilePermissions path = do
    perms <- getPermissions path
    return $ FilePermissions 
        { readable = readable perms
        , writable = writable perms
        , executable = executable perms
        }

setFilePermissions :: FilePath -> FilePermissions -> IO ()
setFilePermissions path perms = do
    setPermissions path (Permissions 
        { readable = readable perms
        , writable = writable perms
        , executable = executable perms
        , searchable = searchable perms
        })

-- | File metadata
data FileInfo = FileInfo
    { filePath :: FilePath
    , fileSize :: Integer
    , fileType :: FileType
    , modificationTime :: String
    } deriving (Show)

data FileType = RegularFile | Directory | SymbolicLink | Other
    deriving (Show, Eq)

getFileInfo :: FilePath -> IO FileInfo
getFileInfo path = do
    size <- getFileSize path >>= maybe (return 0) return
    isDir <- doesDirectoryExist path
    isSym <- pathIsSymbolicLink path
    mtime <- getModificationTime path >>= return . show
    let ftype = if isDir then Directory else if isSym then SymbolicLink else RegularFile
    return $ FileInfo path size ftype mtime

-- | Search in file
searchInFile :: FilePath -> String -> IO [String]
searchInFile path pattern = do
    lines <- readFileLines path
    return $ filter (pattern `isInfixOf`) lines

-- | Count lines in file
countLines :: FilePath -> IO Int
countLines path = do
    content <- readFile' path
    return $ length (lines content)

-- | Count words in file
countWords :: FilePath -> IO Int
countWords path = do
    content <- readFile' path
    return $ length (words content)

-- | Count characters in file
countCharacters :: FilePath -> IO Int
countCharacters path = do
    content <- readFile' path
    return $ length content

-- | Process CSV file
readCSV :: FilePath -> IO [[String]]
readCSV path = do
    lines <- readFileLines path
    return $ map (splitOn ',') lines

splitOn :: Char -> String -> [String]
splitOn delim str = 
    case dropWhile (== delim) str of
        "" -> []
        s -> w : splitOn delim (tail (dropWhile (/= delim) s))
        where
            w = takeWhile (/= delim) s

writeCSV :: FilePath -> [[String]] -> IO ()
writeCSV path rows = do
    let content = unlines [ intercalate "," row | row <- rows ]
    writeFile' path content

-- | Process JSON file (simplified)
readJSON :: FilePath -> IO String
readJSON path = readFile' path

writeJSON :: FilePath -> String -> IO ()
writeJSON path content = writeFile' path content

-- | Process XML file (simplified)
readXML :: FilePath -> IO String
readXML path = readFile' path

writeXML :: FilePath -> String -> IO ()
writeXML path content = writeFile' path content

-- | Watch file for changes
watchFile :: FilePath -> IO ()
watchFile path = do
    initialTime <- getModificationTime path
    forever $ do
        threadDelay 1000000  -- Check every second
        currentTime <- getModificationTime path
        when (currentTime /= initialTime) $ do
            putStrLn $ "File changed: " ++ path
            return ()

-- | Batch file processing
processBatch :: [FilePath] -> (FilePath -> IO ()) -> IO ()
processBatch files action = mapM_ action files

-- | Parallel file processing
processFilesParallel :: Int -> [FilePath] -> (FilePath -> IO ()) -> IO ()
processFilesParallel maxThreads files action = do
    sem <- newQSem maxThreads
    forM_ files $ \file -> do
        waitQSem sem
        forkIO $ finally (action file) (signalQSem sem)
    threadDelay 1000000

-- | Helper functions
randomNumber :: Int -> Int
randomNumber n = n  -- Placeholder

intercalate :: [a] -> [[a]] -> [a]
intercalate sep [] = []
intercalate sep [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` subsequences haystack
    where subsequences [] = [[]]
          subsequences (x:xs) = [x:sub | sub <- subsequences xs] ++ subsequences xs

-- | main function
main :: IO ()
main = do
    putStrLn "=== Haskell IO Operations Demo ==="
    
    -- File operations
    putStrLn "\n--- File Operations ---"
    writeFile' "test.txt" "Hello, World!"
    content <- readFile' "test.txt"
    putStrLn $ "Content: " ++ content
    
    -- Line operations
    putStrLn "\n--- Line Operations ---"
    writeFileLines "lines.txt" ["Line 1", "Line 2", "Line 3"]
    lines <- readFileLines "lines.txt"
    print lines
    
    -- Binary file operations
    putStrLn "\n--- Binary File Operations ---"
    let binaryData = BS.pack [0..255]
    writeBinaryFile "binary.bin" binaryData
    loadedData <- readBinaryFile "binary.bin"
    putStrLn $ "Binary data size: " ++ show (BS.length loadedData)
    
    -- Directory operations
    putStrLn "\n--- Directory Operations ---"
    createDirectoryRecursive "test_dir/subdir"
    writeFile' "test_dir/file.txt" "Test content"
    files <- listDirectory "test_dir"
    putStrLn $ "Files in test_dir: " ++ show files
    
    -- CSV operations
    putStrLn "\n--- CSV Operations ---"
    let csvData = [["Name", "Age"], ["Alice", "30"], ["Bob", "25"]]
    writeCSV "data.csv" csvData
    loadedCSV <- readCSV "data.csv"
    print loadedCSV
    
    -- File info
    putStrLn "\n--- File Info ---"
    info <- getFileInfo "test.txt"
    print info
    
    -- Search in file
    putStrLn "\n--- Search in File ---"
    writeFile' "search.txt" "Hello World\nGoodbye World\nHello Again"
    results <- searchInFile "search.txt" "Hello"
    putStrLn $ "Found: " ++ show results
    
    -- Statistics
    putStrLn "\n--- File Statistics ---"
    lineCount <- countLines "test.txt"
    wordCount <- countWords "test.txt"
    charCount <- countCharacters "test.txt"
    putStrLn $ "Lines: " ++ show lineCount
    putStrLn $ "Words: " ++ show wordCount
    putStrLn $ "Characters: " ++ show charCount
    
    -- Cleanup
    putStrLn "\n--- Cleanup ---"
    removeFile' "test.txt"
    removeFile' "lines.txt"
    removeFile' "binary.bin"
    removeFile' "data.csv"
    removeFile' "search.txt"
    removeDirectoryRecursive "test_dir"
    putStrLn "Cleanup complete"
    
    putStrLn "\n=== IO Operations Demo Complete ==="

