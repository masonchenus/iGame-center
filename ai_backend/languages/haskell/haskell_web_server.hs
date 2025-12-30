
-- Haskell Web Server - HTTP Server Implementation
{-# LANGUAGE OverloadedStrings #-}

import Network.Socket
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.ByteString (ByteString)
import Control.Exception
import Control.Monad
import Data.List
import Data.Char
import System.IO
import Text.Printf

-- | HTTP Request
data HTTPRequest = HTTPRequest
    { method :: String
    , path :: String
    , version :: String
    , headers :: [(String, String)]
    , body :: Maybe ByteString
    } deriving (Show)

-- | HTTP Response
data HTTPResponse = HTTPResponse
    { statusCode :: Int
    , statusMessage :: String
    , responseHeaders :: [(String, String)]
    , responseBody :: Maybe ByteString
    } deriving (Show)

-- | Route handler type
type RouteHandler = HTTPRequest -> IO HTTPResponse

-- | Simple router
data Router = Router
    { routes :: [(String, (String -> RouteHandler))]
    , notFoundHandler :: RouteHandler
    }

emptyRouter :: Router
emptyRouter = Router [] defaultNotFound

defaultNotFound :: RouteHandler
defaultNotFound _ = return $ HTTPResponse 404 "Not Found" [] Nothing

addRoute :: String -> String -> RouteHandler -> Router -> Router
addRoute method path handler router = 
    router { routes = ((method, path), handler) : routes router }

get :: String -> RouteHandler -> Router -> Router
get = addRoute "GET"

post :: String -> RouteHandler -> Router -> Router
post = addRoute "POST"

route :: Router -> HTTPRequest -> IO HTTPResponse
route router request = do
    let key = (method request, path request)
    case lookup key (routes router) of
        Just handler -> handler request
        Nothing -> notFoundHandler router request

-- | HTTP parser
parseRequest :: ByteString -> Maybe HTTPRequest
parseRequest bytes = do
    let (headerPart, body) = BS.breakSubstring "\r\n\r\n" bytes
    let headerLines = BS.split 13 headerPart  -- 13 = '\r'
    let (requestLine : headerLines') = map BS.unpack headerLines
    let [method, path, version] = words requestLine
    let headers = parseHeaders headerLines'
    let body' = if BS.null body then Nothing else Just (BS.drop 4 body)
    Just HTTPRequest { method = method, path = path, version = version, headers = headers, body = body' }

parseHeaders :: [String] -> [(String, String)]
parseHeaders [] = []
parseHeaders (line:lines) =
    case break (== ':') line of
        (name, ':':value) -> (name, tail value) : parseHeaders lines
        _ -> parseHeaders lines

-- | HTTP response builder
buildResponse :: HTTPResponse -> ByteString
buildResponse response = 
    let statusLine = BS.concat [BS.pack (version response), BS.pack " ", 
                                BS.pack (show (statusCode response)), BS.pack " ",
                                BS.pack (statusMessage response), BS.pack "\r\n"]
        headerLines = BS.concat [BS.pack $ name ++ ": " ++ value ++ "\r\n" | (name, value) <- responseHeaders response]
        body = case responseBody response of
                Just b -> BS.concat [BS.pack "\r\n", b]
                Nothing -> BS.pack "\r\n"
    in BS.concat [statusLine, headerLines, BS.pack "\r\n", body]

-- | Simple route handlers
helloHandler :: RouteHandler
helloHandler _ = return $ HTTPResponse 200 "OK" 
    [("Content-Type", "text/plain")] 
    (Just "Hello, World!")

jsonHandler :: RouteHandler
jsonHandler _ = return $ HTTPResponse 200 "OK"
    [("Content-Type", "application/json")]
    (Just "{\"message\": \"Hello, World!\", \"status\": \"success\"}")

htmlHandler :: RouteHandler
htmlHandler _ = return $ HTTPResponse 200 "OK"
    [("Content-Type", "text/html")]
    (Just "<html><body><h1>Hello, World!</h1></body></html>")

echoHandler :: RouteHandler
echoHandler request = return $ HTTPResponse 200 "OK"
    [("Content-Type", "text/plain")]
    (body request)

staticHandler :: FilePath -> RouteHandler
staticHandler baseDir request = do
    let filePath = baseDir ++ path request
    content <- catch (Just <$> BS.readFile filePath) (\(_ :: SomeException) -> return Nothing)
    case content of
        Just bytes -> return $ HTTPResponse 200 "OK" 
            [("Content-Type", getContentType filePath)] 
            (Just bytes)
        Nothing -> return $ HTTPResponse 404 "Not Found" [] Nothing

getContentType :: FilePath -> String
getContentType path
    | ".html" `isSuffixOf` path = "text/html"
    | ".css" `isSuffixOf` path = "text/css"
    | ".js" `isSuffixOf` path = "application/javascript"
    | ".json" `isSuffixOf` path = "application/json"
    | ".png" `isSuffixOf` path = "image/png"
    | ".jpg" `isSuffixOf` path = "image/jpeg"
    | ".gif" `isSuffixOf` path = "image/gif"
    | otherwise = "application/octet-stream"

-- | Query parameter parsing
getQueryParam :: HTTPRequest -> String -> Maybe String
getQueryParam request param = do
    let queryString = case break (== '?') (path request) of
            (_, '?':qs) -> qs
            _ -> ""
    let params = breakBoth '&' queryString
    lookup param params

breakBoth :: Char -> String -> [(String, String)]
breakBoth delim str = map (break (== '=')) (split delim str)

split :: Char -> String -> [String]
split _ [] = []
split delim str = 
    case break (== delim) str of
        (before, []) -> [before]
        (before, _:after) -> before : split delim after

-- | Server implementation
type Server = Socket

createServer :: Int -> IO Socket
createServer port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
    listen sock 5
    return sock

handleClient :: Router -> Socket -> IO ()
handleClient router sock = do
    (conn, _) <- accept sock
    _ <- forkIO $ handleConnection router conn
    handleClient router sock

handleConnection :: Router -> Socket -> IO ()
handleConnection router conn = do
    request <- recv conn 4096
    unless (BS.null request) $ do
        case parseRequest request of
            Just req -> do
                response <- route router req
                send conn (buildResponse response)
            Nothing -> do
                let errorResponse = buildResponse $ HTTPResponse 400 "Bad Request" [] Nothing
                send conn errorResponse
    close conn

-- | HTTP client
httpGet :: String -> Int -> String -> IO (Maybe ByteString)
httpGet host port path = do
    sock <- socket AF_INET Stream 0
    connect sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
    let request = BS.concat [BS.pack "GET ", BS.pack path, BS.pack " HTTP/1.1\r\nHost: ", 
                             BS.pack host, BS.pack "\r\nConnection: close\r\n\r\n"]
    send sock request
    response <- recv sock 4096
    close sock
    return $ Just response

httpPost :: String -> Int -> String -> ByteString -> IO (Maybe ByteString)
httpPost host port path body = do
    sock <- socket AF_INET Stream 0
    connect sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
    let request = BS.concat [BS.pack "POST ", BS.pack path, BS.pack " HTTP/1.1\r\nHost: ",
                             BS.pack host, BS.pack "\r\nContent-Length: ", 
                             BS.pack (show (BS.length body)), 
                             BS.pack "\r\nContent-Type: application/json\r\n\r\n", body]
    send sock request
    response <- recv sock 4096
    close sock
    return $ Just response

-- | Middleware
type Middleware = RouteHandler -> RouteHandler

loggingMiddleware :: Middleware
loggingMiddleware handler request = do
    putStrLn $ method request ++ " " ++ path request
    handler request

corsMiddleware :: Middleware
corsMiddleware handler request = do
    response <- handler request
    return response { responseHeaders = ("Access-Control-Allow-Origin", "*") : responseHeaders response }

jsonContentMiddleware :: Middleware
jsonContentMiddleware handler request = do
    response <- handler request
    let newHeaders = ("Content-Type", "application/json") : filter (\(k, _) -> k /= "Content-Type") (responseHeaders response)
    return response { responseHeaders = newHeaders }

-- | Simple router builder
router :: Router
router = emptyRouter
    & get "/hello" helloHandler
    & get "/json" jsonHandler
    & get "/html" htmlHandler
    & post "/echo" echoHandler
    & get "/static/*" (staticHandler "./static")

(&) :: Router -> (Router -> Router) -> Router
(&) r f = f r

-- | main function
main :: IO ()
main = do
    putStrLn "=== Haskell Web Server Demo ==="
    
    -- Create server
    putStrLn "\n--- Starting Server ---"
    server <- createServer 8080
    putStrLn "Server listening on port 8080"
    
    -- Handle requests
    handleClient router server
    
    -- Cleanup
    close server
    putStrLn "Server stopped"

-- | Demo client
clientDemo :: IO ()
clientDemo = do
    putStrLn "\n--- HTTP Client Demo ---"
    
    -- GET request
    putStrLn "Sending GET request..."
    response <- httpGet "localhost" 8080 "/hello"
    print response
    
    -- POST request
    putStrLn "Sending POST request..."
    postResponse <- httpPost "localhost" 8080 "/echo" "{\"test\": \"data\"}"
    print postResponse

-- | Advanced features
data HTTPServer = HTTPServer
    { serverSocket :: Socket
    , serverRouter :: Router
    , middleware :: [Middleware]
    }

startServer :: Int -> Router -> [Middleware] -> IO HTTPServer
startServer port router middleware = do
    sock <- createServer port
    return $ HTTPServer sock router middleware

runServer :: HTTPServer -> IO ()
runServer server = handleClient (applyMiddleware (serverRouter server) (middleware server)) (serverSocket server)

applyMiddleware :: Router -> [Middleware] -> Router
applyMiddleware router [] = router
applyMiddleware router (m:ms) = applyMiddleware (m router) ms

