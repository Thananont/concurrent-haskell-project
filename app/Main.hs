module Main (main) where

import Client (initClient)
import Server (initServer)
import Types (Request(..), RequestQueue(..))
import Control.Concurrent
import Control.Monad (forM_)
import Data.IORef (newIORef)
import System.Log.FastLogger (newFileLoggerSet, defaultBufSize)


main :: IO ()
main = do 
    logger <- newFileLoggerSet defaultBufSize "requests.log"

    processedCounter <- newIORef 0
    
    let q1 = EmptyQueue :: RequestQueue Request
    forServer <- newMVar q1
    end <- newEmptyMVar
    
    forM_ [0..9] $ \i -> forkIO (initClient i forServer 10)
    
    forM_ [1..3] $ \i -> forkIO (initServer i forServer processedCounter end logger)
    
    endSignal <- takeMVar end
    print endSignal

