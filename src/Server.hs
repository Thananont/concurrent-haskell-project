module Server
    ( initServer
    ) where

import Queue (RequestQueue, enqueue, dequeue)
import Types (Request(..), Response(..))
import Control.Concurrent (MVar, takeMVar, newMVar, newEmptyMVar, putMVar, threadDelay)
import System.Random
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.IORef (IORef, readIORef, writeIORef)

initServer :: Int -> MVar (RequestQueue Request) -> IORef Int -> MVar String -> IO ()
initServer name forServer processedCounter end = do 
    queueLog <- takeMVar forServer
    print (show name ++ "called")
    let (request, queue) = dequeue queueLog
    placeHolderForDefault <- newEmptyMVar
    currentTime <- getCurrentTime
    let (requestDetail, responseSignal, requestTime) = parseRequest request placeHolderForDefault currentTime
    print requestDetail
    print (show currentTime)
    let diff = diffUTCTime currentTime requestTime
    putMVar forServer queue 
    let response = Response {
        responseData = "The ping was received and it took the server " ++ show diff ++ " to respond.",
        responseTime = currentTime
    }
    putMVar responseSignal response

    currentCount <- readIORef processedCounter
    let newCount = if requestDetail == "no request" then currentCount else currentCount + 1
    writeIORef processedCounter newCount
    putStrLn (show newCount ++ show name)
    if newCount >= 100
        then do
            putMVar end "Server terminated"
        else return ()
    threadDelay 500000
    initServer name forServer processedCounter end

parseRequest :: Maybe Request -> MVar Response -> UTCTime -> (String, MVar Response, UTCTime)
parseRequest (Just request) _ _ = (requestDetail request, responseSignal request, requestTime request)
parseRequest Nothing defaultMVar defaultTime = ("no request", defaultMVar, defaultTime)