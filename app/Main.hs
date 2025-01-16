module Main (main) where

import Lib
import Client
import Server
import Functions (RequestQueue(..), enqueue, dequeue)
import Control.Concurrent
import Control.Monad (forM_)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.IORef (newIORef)

-- main :: IO ()
-- main = someFunc

-- main = do
--     coin <- coinFlip
--     putStrLn $ "Random coin is: " ++ (show coin)
--     box <- newMVar coin
--     winner <- newEmptyMVar
--     forkIO (process "A" winner box)
--     -- forkIO (process "B" winner box)
--     -- forkIO (process "C" winner box)
--     w <- takeMVar winner
--     putStrLn $ w

main = do 
    processedCounter <- newIORef 0
    let q1 = EmptyQueue :: RequestQueue String (MVar String) String
    -- coin <- coinFlip
    -- forClient <- newMVar q1
    forServer <- newMVar q1
    end <- newEmptyMVar
    -- serverAvailable <- newEmptyMVar

    -- forkIO (initServer "test2" forServer processedCounter end)

    -- forkIO (initServer "test2" forServer end)
    print "next"
    forM_ [0..9] $ \i -> 
        forkIO (initClient i forServer 10)
    -- forkIO (initClient 0 forServer 10)
    -- forkIO (initClient 1 forServer 10)
    -- forkIO (initClient 2 forServer 10)
    -- forkIO (initClient 3 forServer 10)
    -- forkIO (initClient 4 forServer 10)
    -- forkIO (initClient 5 forServer 10)
    -- forkIO (initClient 6 forServer 10)
    -- forkIO (initClient 7 forServer 10)
    -- forkIO (initClient 8 forServer 10)
    -- forkIO (initClient 9 forServer 10)
    forkIO (initServer "test0" forServer processedCounter end)
    forkIO (initServer "test1" forServer processedCounter end)
    forkIO (initServer "test2" forServer processedCounter end)

    w <- takeMVar end
    putStrLn "Ended"

