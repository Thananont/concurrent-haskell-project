module Types
    ( Request(..)
    , Response(..) 
    ) where

import Control.Concurrent (MVar)
import Data.Time.Clock (UTCTime)

data Request = Request {
    requestDetail :: String,
    responseSignal :: MVar Response,
    requestTime :: UTCTime
}

data Response = Response { 
    responseData :: String,
    responseTime :: UTCTime
}