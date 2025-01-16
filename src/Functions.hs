module Functions
    ( RequestQueue(..), 
    enqueue,
    dequeue
    ) where

import Types (Request, Response)

data RequestQueue a = EmptyQueue | Queue a (RequestQueue a) deriving (Show, Eq)

enqueue :: a -> RequestQueue a -> RequestQueue a
enqueue a EmptyQueue = Queue a EmptyQueue
enqueue a (Queue b c) = Queue a (enqueue b c)

dequeue :: RequestQueue a -> (Maybe a, RequestQueue a)
dequeue EmptyQueue = (Nothing, EmptyQueue) -- Nothing to dequeue if empty
dequeue (Queue a b) = (Just a, b)     
