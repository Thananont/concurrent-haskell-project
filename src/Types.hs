module Types
    ( Request(..)
    , Response(..) 
    ) where

import Control.Concurrent (MVar)

data Request = Request {
    requestDetail :: String,
    responseSignal :: MVar String,
    requestTime :: String
}

data Response = Response { 
    responseData :: String,
    responseTime :: String
}