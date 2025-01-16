module Main (main) where

import Lib
import Client
import Server
import Control.Concurrent

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
    coin <- coinFlip
    forClient <- newMVar coin
    forServer <- newEmptyMVar
    end <- newEmptyMVar
    -- serverAvailable <- newEmptyMVar
    forkIO (initServer "test" forClient forServer end)
    forkIO (initClient "test1" forClient forServer 10)
    w <- takeMVar end
    putStrLn "Ended"

