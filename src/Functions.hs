module Functions
    ( ServerQueue(..), 
    enqueue,
    dequeue,
    isEmpty
    ) where

data ServerQueue a b = EndQueue | EmptyQueue | Queue a b (ServerQueue a b) deriving (Show, Eq)

enqueue :: a -> b -> ServerQueue a b -> ServerQueue a b
enqueue x y EmptyQueue = Queue x y EmptyQueue
enqueue x y (Queue q w r) = Queue q y (enqueue x w r)

dequeue :: ServerQueue a b -> (Maybe (a, b), ServerQueue a b)
dequeue EmptyQueue = (Nothing, EmptyQueue) -- Nothing to dequeue if empty
dequeue (Queue x y q) = (Just (x, y), q)     

isEmpty :: ServerQueue a b -> Bool
isEmpty EmptyQueue = True
isEmpty _ = False