module Client
    ( initClient
    ) where

import Lib
import Control.Concurrent (MVar, takeMVar, putMVar, threadDelay)
import System.Random
import Data.Time.Clock (getCurrentTime)

initClient :: String -> MVar Coin -> MVar Coin -> Int -> IO ()
initClient name forClient forServer limit = 
    if limit == 0 
        then return ()
        else do
            c1 <- takeMVar forClient
            putStrLn $ name ++ "Client is doing abc"
            putStrLn $ (show limit)
            threadDelay 5000000
            putMVar forServer (c1)
            -- threadDelay 5000000
            initClient name forClient forServer (limit - 1)