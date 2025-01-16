module Client
    ( initClient
    ) where

import Lib
import Functions 
import Control.Concurrent (MVar, takeMVar, putMVar, threadDelay)
import System.Random
import Data.Time.Clock (UTCTime, getCurrentTime)

initClient :: Int -> MVar (ServerQueue Int UTCTime) -> Int -> IO ()
initClient id forServer limit = 
    if limit == 0 
        then return ()
        else do
            -- print "called1"
            c1 <- takeMVar forServer
            time <- getCurrentTime 
            let temp = enqueue (limit + (id * 10)) time c1
            putStrLn $ (show id) ++ "Client is doing abc"
            putStrLn $ (show limit)
            -- threadDelay 5000
            putStrLn $ "Client ping at " ++ (show time)
            putMVar forServer (temp)
            -- threadDelay 5000000
            print "done1"
            initClient id forServer (limit - 1)