module Main (main) where

import Lib
import Client
import Server
import Functions (ServerQueue(..), enqueue, dequeue, isEmpty)
import Control.Concurrent
import Data.Time.Clock (UTCTime, getCurrentTime)

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
    let q1 = EmptyQueue :: ServerQueue Int UTCTime
    -- coin <- coinFlip
    -- forClient <- newMVar q1
    forServer <- newMVar q1
    end <- newEmptyMVar
    -- serverAvailable <- newEmptyMVar
    forkIO (initServer "test" forServer end)
    -- forkIO (initServer "test2" forServer end)
    forkIO (initClient 1 forServer 10)
    -- forkIO (initClient "test1" q1 forServer 10)
    w <- takeMVar end
    putStrLn "Ended"

