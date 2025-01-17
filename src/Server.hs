module Server
    ( initServer
    ) where

import Queue (RequestQueue, dequeue)
import Types (Request(..), Response(..))
import Control.Concurrent (MVar, takeMVar, newEmptyMVar, putMVar, threadDelay)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.IORef (IORef, atomicModifyIORef)
import System.Log.FastLogger 

initServer :: Int -> MVar (RequestQueue Request) -> IORef Int -> MVar String -> LoggerSet-> IO ()
initServer serverId forServer processedCounter end logger = do 
    queueLog <- takeMVar forServer
    let (request, queue) = dequeue queueLog

    placeHolderForDefault <- newEmptyMVar
    
    currentTime <- getCurrentTime
    let (reqDetail, reqSignal, reqTime) = parseRequest request placeHolderForDefault currentTime
    let diff = diffUTCTime currentTime reqTime
    
    putMVar forServer queue 
    
    let response = Response {
        responseData = "The ping was received and it took the server " ++ show diff ++ " to respond.",
        responseCode = 200,
        responseTime = currentTime
    }
    
    logRequest logger serverId currentTime reqDetail

    putMVar reqSignal response

    requestCount <- atomicModifyIORef processedCounter $ \currentCount ->
        let newCount = if reqDetail == "no request" then currentCount else currentCount + 1
        in (newCount, newCount)

    if requestCount >= 100
        then do
            putMVar end "Server terminated"
        else return ()
    
    threadDelay 500000
    initServer serverId forServer processedCounter end logger

parseRequest :: Maybe Request -> MVar Response -> UTCTime -> (String, MVar Response, UTCTime)
parseRequest (Just request) _ _ = (requestDetail request, responseSignal request, requestTime request)
parseRequest Nothing defaultMVar defaultTime = ("no request", defaultMVar, defaultTime)

logRequest :: LoggerSet -> Int -> UTCTime -> String -> IO ()
logRequest logger serverId date message = do
    if message == "no request"
        then return ()
        else do
            let logMessage = "[" ++ show date ++ "]ServerId" ++ (show serverId) ++ ": " ++ message
            print logMessage
            pushLogStrLn logger (toLogStr logMessage)