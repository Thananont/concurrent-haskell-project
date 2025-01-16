module Functions
    ( RequestQueue(..), 
    enqueue,
    dequeue
    ) where

data RequestQueue a b c = EmptyQueue | Queue a b c (RequestQueue a b c) deriving (Show, Eq)

enqueue :: a -> b -> c -> RequestQueue a b c -> RequestQueue a b c
enqueue x y z EmptyQueue = Queue x y z EmptyQueue
enqueue x y z (Queue q w e r) = Queue x y z (enqueue q w e r)

dequeue :: RequestQueue a b c -> (Maybe (a, b, c), RequestQueue a b c)
dequeue EmptyQueue = (Nothing, EmptyQueue) -- Nothing to dequeue if empty
dequeue (Queue x y z q) = (Just (x, y, z), q)     
