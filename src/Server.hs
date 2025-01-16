module Server
    ( initServer
    ) where

import Lib
import Control.Concurrent (MVar, takeMVar, putMVar, threadDelay)
import System.Random
import Data.Time.Clock (getCurrentTime)

initServer :: String -> MVar Coin -> MVar Coin -> MVar Coin -> IO ()
initServer name forClient forServer end = do 
    c1 <- takeMVar forServer
    putStrLn $ name ++ "Server is doing abc"
    threadDelay 5000
    putMVar forClient c1
    -- threadDelay 5000000
    initServer name forClient forServer end