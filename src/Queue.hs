module Queue
    ( enqueue
    , dequeue
    ) where

import Types (RequestQueue(..))

-- | The function takes an input and add it infront of the Request queue and returns said queue.
enqueue :: a -> RequestQueue a -> RequestQueue a
enqueue a EmptyQueue = Queue a EmptyQueue
enqueue a (Queue b c) = Queue a (enqueue b c)

-- | The function takes a RequestQueue and removes the outmost item of the queue and returns said item along 
-- with the queue.
dequeue :: RequestQueue a -> (Maybe a, RequestQueue a)
dequeue EmptyQueue = (Nothing, EmptyQueue) 
dequeue (Queue a b) = (Just a, b)     
