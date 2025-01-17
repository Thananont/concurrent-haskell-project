module Client
    ( initClient
    ) where

import Queue (RequestQueue, enqueue)
import Types (Request(..), Response(..))
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar, threadDelay)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime)

initClient :: Int -> MVar (RequestQueue Request) -> Int -> IO ()
initClient clientId forServer limit = 
    if limit == 0 
        then return ()
        else do
            queueLog <- takeMVar forServer
            resSignal <- newEmptyMVar

            let clientIdString = show clientId
            time <- getCurrentTime 
            
            let clientRequestDetail = "ClientId " ++ clientIdString ++ " pings the server at " ++ (show time)
            let request = Request {
                requestDetail = clientRequestDetail,
                responseSignal = resSignal,
                requestTime = time
            }
            let requestQueue = enqueue request queueLog
            
            print ("ClientId " ++ clientIdString ++ ": Pings the server")
            putMVar forServer (requestQueue)
            
            response <- takeMVar resSignal
            let resData = parseResponse response
            print ("ClientId " ++ clientIdString ++ ": " ++ resData) 
            waitTime <- randomRIO (1000000 :: Int, 10000000 :: Int)
            threadDelay waitTime
            initClient clientId forServer (limit - 1)

parseResponse :: Response -> String
parseResponse response = responseData response        