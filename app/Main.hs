module Main (main) where

import Client
import Server
import Functions (RequestQueue(..))
import Types (Request(..))
import Control.Concurrent
import Control.Monad (forM_)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.IORef (newIORef)


main :: IO ()
main = do 
    processedCounter <- newIORef 0
    let q1 = EmptyQueue :: RequestQueue Request
    -- forClient <- newMVar q1
    forServer <- newMVar q1
    end <- newEmptyMVar
    -- serverAvailable <- newEmptyMVar

    -- forkIO (initServer "test2" forServer processedCounter end)

    -- forkIO (initServer "test2" forServer end)
    print "next"
    forM_ [0..9] $ \i -> 
        forkIO (initClient i forServer 10)
    -- forkIO (initServer "test0" forServer processedCounter end)
    -- forkIO (initServer "test1" forServer processedCounter end)
    -- forkIO (initServer "test2" forServer processedCounter end)
    forM_ [1..3] $ \i -> 
        forkIO (initServer i forServer processedCounter end)

    w <- takeMVar end
    putStrLn "Ended"

