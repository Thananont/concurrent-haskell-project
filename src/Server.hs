module Server
    ( initServer
    ) where

import Lib
import Functions (ServerQueue(..), enqueue, dequeue, isEmpty)
import Control.Concurrent (MVar, takeMVar, putMVar, threadDelay)
import System.Random
import Data.Time.Clock (UTCTime, getCurrentTime)

initServer :: String -> MVar (ServerQueue Int UTCTime) -> MVar Int -> IO ()
initServer name forServer end = do 
    c1 <- takeMVar forServer
    print "called"
    putStrLn $ (show c1)
    let (request, queue) = dequeue c1
    -- putStrLn $ (show request)
    if queue == EmptyQueue 
        then do
            print "then"
            putMVar forServer EmptyQueue
            putStrLn $ (show request)
            putStrLn $ (show queue)
        else do
            let (id, date) = parseRequest request
            print "else"
            putMVar forServer queue 
            print "after"   
            putStrLn $ name ++ "Server is doing abc"
            putStrLn $ (show id)
            putStrLn $ (show date)
            putStrLn $ "Server is pinged at " ++ (show request)
            -- putStrLn $ (show queue)
            -- putStrLn $ (show forServer)
            if id == 1
                then putMVar end 1
                else print "Whoops"
    threadDelay 50000
    print "done"
    initServer name forServer end

parseRequest :: Maybe (Int, UTCTime) -> (Int, UTCTime)
parseRequest (Just (a, b)) = (a, b)
parseReuqest Nothing = error "parseRequest: Nothing value cannot be parsed"