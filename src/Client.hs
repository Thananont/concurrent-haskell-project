module Client
    ( initClient
    ) where

import Lib
import Functions 
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar, threadDelay)
import System.Random
import Data.Time.Clock (UTCTime, getCurrentTime)

initClient :: Int -> MVar (RequestQueue String (MVar String) String) -> Int -> IO ()
initClient id forServer limit = 
    if limit == 0 
        then return ()
        else do
            c1 <- takeMVar forServer
            print (show id ++ "called1")
            responseSignal <- newEmptyMVar
            print "1"
            time <- getCurrentTime 
            print "2"
            let temp = enqueue (show (limit + (id * 10))) responseSignal (show time) c1
            print "3"
            -- putStrLn $ (show id) ++ "Client is doing abc"
            -- putStrLn $ (show limit)
            -- threadDelay 5000
            -- putStrLn $ "Client ping at " ++ (show time)
            putMVar forServer (temp)
            print "4"
            response <- takeMVar responseSignal
            print (show id ++ "response received")
            -- print response
            -- threadDelay 5000000
            -- print "done1"
            threadDelay 500000
            initClient id forServer (limit - 1)