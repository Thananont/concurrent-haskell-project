module Client
    ( initClient
    ) where

import Queue (RequestQueue, enqueue)
import Types (Request(..), Response(..))
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar, threadDelay)
import System.Random (randomRIO)
import Data.Time.Clock (UTCTime, getCurrentTime)

initClient :: Int -> MVar (RequestQueue Request) -> Int -> IO ()
initClient clientId forServer limit = 
    if limit == 0 
        then return ()
        else do
            queueLog <- takeMVar forServer
            resSignal <- newEmptyMVar

            time <- getCurrentTime 
            
            let clientRequestDetail = "ClientId " ++ (show clientId) ++ " pings the server at " ++ (show time)
            let request = Request {
                requestDetail = clientRequestDetail,
                responseSignal = resSignal,
                requestTime = time
            }
            let requestQueue = enqueue request queueLog
            
            print ("ClientId " ++ (show clientId) ++ ": Pings the server")
            putMVar forServer (requestQueue)
            
            response <- takeMVar resSignal
            let (resData, resTime) = parseResponse response
            print ("ClientId " ++ (show clientId) ++ ": " ++ resData) 
            waitTime <- randomRIO (1000000 :: Int, 10000000 :: Int)
            threadDelay waitTime
            initClient clientId forServer (limit - 1)

parseResponse :: Response -> (String, UTCTime)
parseResponse response = (responseData response, responseTime response)        