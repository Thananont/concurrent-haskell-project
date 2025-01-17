module Client
    ( initClient
    , parseResponse
    ) where

import Queue (enqueue)
import Types (Request(..), Response(..), RequestQueue(..))
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar, threadDelay)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime)

-- | This is a function initializes a client and send a ping requests to the specified queue. After sending the request,
-- the client then wait for the response from the server. After receiving a response, the client waits a random 
-- amount of time between one to ten second before repeating the process. This process is repeated based on the limit 
-- input.
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

-- | The function parses a response and returns the details of the response. 
parseResponse :: Response -> String
parseResponse response = responseData response        