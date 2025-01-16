module Lib
    ( someFunc,
    coinFlip,
    process,
    Coin
    ) where

import Control.Concurrent (MVar, takeMVar, putMVar)
import System.Random
import Data.Time.Clock (getCurrentTime)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Coin = Head | Tail deriving (Show, Eq)

coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail

process :: String -> MVar String -> MVar Coin -> IO ()
process name winner box = do
    c1 <- takeMVar box
    putStrLn $ name ++ " 's turn"
    c2 <- coinFlip
    putStrLn $ " -- got " ++ (show c2)
    if c1 == c2 then
        putMVar winner $ "Process " ++ name ++ " wins!"
    else do
        putStrLn $ " -- putting coin back in the box"
        time <- getCurrentTime 
        putStrLn $ (show time)
        putMVar box c1
        -- threadDelay 5
        process name winner box