module Client
    ( initClient
    ) where

import Queue (RequestQueue, enqueue)
import Types (Request(..), Response(..))
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar, threadDelay)
import System.Random (randomRIO)
import Data.Time.Clock (UTCTime, getCurrentTime)

initClient :: Int -> MVar (RequestQueue Request) -> Int -> IO ()
initClient id forServer limit = 
    if limit == 0 
        then return ()
        else do
            queueLog <- takeMVar forServer
            responseSignal <- newEmptyMVar

            time <- getCurrentTime 
            
            let clientRequestDetail = "ClientId " ++ (show id) ++ " pings the server at " ++ (show time)
            let request = Request {
                requestDetail = clientRequestDetail,
                responseSignal = responseSignal,
                requestTime = time
            }
            let requestQueue = enqueue request queueLog
            
            putMVar forServer (requestQueue)
            
            response <- takeMVar responseSignal
            let (responseData, responseTime) = parseResponse response
            waitTime <- randomRIO (1000000 :: Int, 10000000 :: Int)
            threadDelay waitTime
            initClient id forServer (limit - 1)

parseResponse :: Response -> (String, UTCTime)
parseResponse response = (responseData response, responseTime response)        