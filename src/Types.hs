module Types
    ( Request(..)
    , Response(..) 
    , RequestQueue(..)
    ) where

import Control.Concurrent (MVar)
import Data.Time.Clock (UTCTime)

-- | Type for request. Contains the detail of the request, the MVar use to respond to the client, and 
-- the UTCTime that the request was made.
data Request = Request {
    requestDetail :: String,
    responseSignal :: MVar Response,
    requestTime :: UTCTime
}

-- | Type for response. Contains the detail of the response, the http status code from the server, and 
-- the UTCTime that the response was sent back.
data Response = Response { 
    responseData :: String,
    responseCode :: Int,
    responseTime :: UTCTime
}

-- | Type for RequestQueue. A type that consists of EmptyQueue or a Queue that contains a RequestQueue. 
data RequestQueue a = EmptyQueue | Queue a (RequestQueue a) deriving (Show)