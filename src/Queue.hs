module Queue
    ( RequestQueue(..), 
    enqueue,
    dequeue
    ) where

data RequestQueue a = EmptyQueue | Queue a (RequestQueue a) deriving (Show)

enqueue :: a -> RequestQueue a -> RequestQueue a
enqueue a EmptyQueue = Queue a EmptyQueue
enqueue a (Queue b c) = Queue a (enqueue b c)

dequeue :: RequestQueue a -> (Maybe a, RequestQueue a)
dequeue EmptyQueue = (Nothing, EmptyQueue) 
dequeue (Queue a b) = (Just a, b)     
