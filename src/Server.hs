module Server
    ( initServer
    ) where

import Lib
import Functions (RequestQueue(..), enqueue, dequeue)
import Control.Concurrent (MVar, takeMVar, newMVar, putMVar, threadDelay)
import System.Random
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.IORef (IORef, readIORef, writeIORef)

initServer :: String -> MVar (RequestQueue String (MVar String) String) -> IORef Int -> MVar Int -> IO ()
initServer name forServer processedCounter end = do 
    c1 <- takeMVar forServer
    print (name ++ "called")
    -- print "1"
    -- putStrLn $ (show c1)
    let (request, queue) = dequeue c1
    placeHolderForDefault <- newMVar "placeHolder"
    -- print "2"
    -- putStrLn $ (show request)
    -- print request
    -- putStrLn $ (show queue)
    -- print queue
    let (id, responseSignal, date) = parseRequest request placeHolderForDefault
    -- print "else"
    -- print "3"
    -- print id
    -- print "4"
    -- print "after"   
    -- putStrLn $ name ++ "Server is doing abc"
    -- putStrLn $ (show id)
    -- putStrLn $ (show date)
    -- putStrLn $ "Server is pinged at " ++ (show request)
    putMVar forServer queue 
    putMVar responseSignal "req res done"

    currentCount <- readIORef processedCounter
    let newCount = if id == "-1" then currentCount else currentCount + 1
    writeIORef processedCounter newCount
    putStrLn (show newCount ++ name)
    -- if newCount >= 100
    --     then putMVar end 1
    --     else print "Whoops"
    if newCount >= 100
        then do
            -- putStrLn (show queue)
            putMVar end 1
        else do
            -- putStrLn (show queue) 
            print "Whoops"
    threadDelay 500000
    print "done"
    initServer name forServer processedCounter end

parseRequest :: Maybe (String, MVar String, String) -> MVar String -> (String, MVar String, String)
parseRequest (Just (a, b, c)) _ = (a, b, c)
parseRequest Nothing defaultMVar = ("-1", defaultMVar ,"no data")