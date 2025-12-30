
-- Haskell Networking - Network Programming and Socket Operations
{-# LANGUAGE OverloadedStrings #-}

import Network.Socket
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString as BS
import Control.Exception
import Control.Monad
import Data.List
import System.IO
import Text.Printf

-- | TCP Client
data TCPClient = TCPClient
    { clientSocket :: Socket
    , clientHost :: String
    , clientPort :: Int
    }

connectTCP :: String -> Int -> IO TCPClient
connectTCP host port = do
    sock <- socket AF_INET Stream 0
    let hints = defaultHints { addrSocketType = Stream }
    addrInfos <- getAddrInfo (Just hints) (Just host) (Just (show port))
    let addr = head addrInfos
    connect sock (addrAddress addr)
    return $ TCPClient sock host port

sendTCP :: TCPClient -> BS.ByteString -> IO Int
sendTCP client data = send (clientSocket client) data

recvTCP :: TCPClient -> Int -> IO BS.ByteString
recvTCP client size = recv (clientSocket client) size

closeTCP :: TCPClient -> IO ()
closeTCP client = close (clientSocket client)

-- | TCP Server
data TCPServer = TCPServer
    { serverSocket :: Socket
    , serverPort :: Int
    }

createTCPServer :: Int -> Int -> IO TCPServer
createTCPServer port backlog = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
    listen sock backlog
    return $ TCPServer sock port

acceptConnection :: TCPServer -> IO (Socket, SockAddr)
acceptConnection server = accept (serverSocket server)

handleClient :: (Socket -> IO ()) -> Socket -> IO ()
handleClient handler sock = do
    _ <- forkIO $ handler sock
    return ()

-- | UDP Client
data UDPClient = UDPClient
    { udpSocket :: Socket
    , udpHost :: String
    , udpPort :: Int
    }

createUDPClient :: String -> Int -> IO UDPClient
createUDPClient host port = do
    sock <- socket AF_INET Datagram 0
    return $ UDPClient sock host port

sendUDP :: UDPClient -> BS.ByteString -> IO ()
sendUDP client data = do
    let hints = defaultHints { addrSocketType = Datagram }
    addrInfos <- getAddrInfo (Just hints) (Just (clientHost client)) (Just (show (clientPort client)))
    let addr = head addrInfos
    sendTo (udpSocket client) data (addrAddress addr)

recvUDP :: UDPClient -> Int -> IO (BS.ByteString, SockAddr)
recvUDP client size = do
    (data, addr) <- recvFrom (udpSocket client) size
    return (data, addr)

-- | HTTP Client
data HTTPRequest = HTTPRequest
    { httpMethod :: String
    , httpPath :: String
    , httpHeaders :: [(String, String)]
    , httpBody :: Maybe BS.ByteString
    }

data HTTPResponse = HTTPResponse
    { httpStatusCode :: Int
    , httpStatusMessage :: String
    , httpResponseHeaders :: [(String, String)]
    , httpResponseBody :: Maybe BS.ByteString
    }

httpGet :: String -> Int -> String -> IO (Maybe HTTPResponse)
httpGet host port path = do
    client <- connectTCP host port
    let request = buildHTTPRequest "GET" path [] Nothing
    _ <- sendTCP client request
    response <- recvHTTPResponse client
    closeTCP client
    return (Just response)

httpPost :: String -> Int -> String -> BS.ByteString -> IO (Maybe HTTPResponse)
httpPost host port path body = do
    client <- connectTCP host port
    let request = buildHTTPRequest "POST" path [("Content-Length", show (BS.length body))] (Just body)
    _ <- sendTCP client request
    response <- recvHTTPResponse client
    closeTCP client
    return (Just response)

buildHTTPRequest :: String -> String -> [(String, String)] -> Maybe BS.ByteString -> BS.ByteString
buildHTTPRequest method path headers body = 
    let headerLines = unlines [method ++ " " ++ path ++ " HTTP/1.1", "Host: localhost"]
                       ++ unlines [k ++ ": " ++ v | (k, v) <- headers]
                       ++ "\r\n"
    in BS.pack (map (fromIntegral . ord) headerLines) `BS.append` fromMaybe BS.empty body

recvHTTPResponse :: TCPClient -> IO HTTPResponse
recvHTTPResponse client = do
    header <- recvUntil client "\r\n\r\n"
    let (statusLine : headerLines) = lines header
        [version, statusCode, statusMessage] = words statusLine
        headers = parseHeaders headerLines
    body <- if hasContentLength headers then do
        let contentLength = read (fromJust (lookup "Content-Length" headers)) :: Int
        recvTCP client contentLength
    else return BS.empty
    return $ HTTPResponse (read statusCode) statusMessage headers (Just body)

recvUntil :: TCPClient -> BS.ByteString -> IO String
recvUntil client delimiter = do
    data <- recvTCP client 1024
    if BS.isSuffixOf delimiter data
        then return (BS.unpack data)
        else do
            rest <- recvUntil client delimiter
            return (BS.unpack data ++ rest)

parseHeaders :: [String] -> [(String, String)]
parseHeaders [] = []
parseHeaders (line:lines) = 
    case break (== ':') line of
        (name, ':':value) -> (name, tail value) : parseHeaders lines
        _ -> parseHeaders lines

hasContentLength :: [(String, String)] -> Bool
hasContentLength headers = isJust (lookup "Content-Length" headers)

-- | Socket options
setSocketTimeout :: Socket -> Int -> IO ()
setSocketTimeout sock microseconds = do
    setSocketOption sock ReadTimeout microseconds

setKeepAlive :: Socket -> Bool -> IO ()
setKeepAlive sock True = setSocketOption sock KeepAlive 1
setKeepAlive sock False = setSocketOption sock KeepAlive 0

-- | Network utilities
getLocalIP :: IO String
getLocalIP = do
    sock <- socket AF_INET Datagram 0
    connect sock (SockAddrInet 80 (tupleToHostAddress (8, 8, 8, 8)))
    getsockname sock >>= return . show
    close sock

getPublicIP :: IO String
getPublicIP = do
    result <- httpGet "api.ipify.org" 80 "/"
    case result of
        Just response -> return (BS.unpack (fromJust (httpResponseBody response)))
        Nothing -> return "Unknown"

-- | Port scanner
scanPort :: String -> Int -> IO Bool
scanPort host port = do
    sock <- socket AF_INET Stream 0
    let hints = defaultHints { addrSocketType = Stream }
    addrInfos <- getAddrInfo (Just hints) (Just host) (Just (show port))
    result <- catch (connect sock (addrAddress (head addrInfos)) >> return True) 
                    (\(_ :: IOException) -> return False)
    close sock
    return result

scanPorts :: String -> [Int] -> IO [(Int, Bool)]
scanPorts host ports = do
    results <- mapM (\port -> do
        isOpen <- scanPort host port
        return (port, isOpen)
    ) ports
    return results

-- | Simple chat server
chatServer :: Int -> IO ()
chatServer port = do
    server <- createTCPServer port 5
    putStrLn $ "Chat server listening on port " ++ show port
    
    clients <- newMVar []
    
    forever $ do
        (conn, addr) <- acceptConnection server
        putStrLn $ "New connection from " ++ show addr
        _ <- forkIO $ handleChatClient conn clients
        return ()

handleChatClient :: Socket -> MVar [Socket] -> IO ()
handleChatClient conn clients = do
    let clientName = "Client-" ++ show (hash conn)
    
    _ <- forkIO $ forever $ do
        msg <- recv conn 1024
        unless (BS.null msg) $ do
            let broadcast = BS.concat [BS.pack "[\"", BS.pack clientName, BS.pack "\"]: ", msg]
            putStrLn $ "Received from " ++ clientName ++ ": " ++ show msg
            return ()
    
    return ()

-- | Network protocols
data Protocol = TCP | UDP | HTTP | HTTPS deriving (Show, Eq)

connectProtocol :: Protocol -> String -> Int -> IO Socket
connectProtocol TCP host port = do
    sock <- socket AF_INET Stream 0
    let hints = defaultHints { addrSocketType = Stream }
    addrInfos <- getAddrInfo (Just hints) (Just host) (Just (show port))
    connect sock (addrAddress (head addrInfos))
    return sock
connectProtocol UDP host port = do
    sock <- socket AF_INET Datagram 0
    return sock
connectProtocol HTTP host port = connectProtocol TCP host port
connectProtocol HTTPS host port = connectProtocol TCP host port

-- | Connection pooling
data ConnectionPool a = ConnectionPool
    { poolConnections :: [a]
    , poolMaxSize :: Int
    , poolFactory :: IO a
    }

createPool :: Int -> IO a -> IO (ConnectionPool a)
createPool maxSize factory = do
    connections <- replicateM maxSize factory
    return $ ConnectionPool connections maxSize factory

getConnection :: ConnectionPool a -> IO a
getConnection pool = do
    return (head (poolConnections pool))

releaseConnection :: ConnectionPool a -> a -> IO ()
releaseConnection pool conn = return ()

-- | main function
main :: IO ()
main = do
    putStrLn "=== Haskell Networking Demo ==="
    
    -- TCP client
    putStrLn "\n--- TCP Client ---"
    putStrLn "TCP client created"
    
    -- TCP server
    putStrLn "\n--- TCP Server ---"
    putStrLn "TCP server ready"
    
    -- HTTP client
    putStrLn "\n--- HTTP Client ---"
    putStrLn "HTTP client ready"
    
    -- Port scanning
    putStrLn "\n--- Port Scanning ---"
    results <- scanPorts "localhost" [80, 443, 8080, 22]
    print results
    
    -- Network utilities
    putStrLn "\n--- Network Utilities ---"
    localIP <- getLocalIP
    putStrLn $ "Local IP: " ++ localIP
    
    -- Chat server
    putStrLn "\n--- Chat Server ---"
    putStrLn "Chat server demo initialized"
    
    putStrLn "\n=== Networking Demo Complete ==="

