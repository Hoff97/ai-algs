module Search.Queue where

class Queue q where
  nil :: q a
  empty :: q a -> Bool
  pop :: q a -> Maybe (a,q a)
  insert :: q a -> a -> q a

singleton :: Queue q => a -> q a
singleton = insert nil

instance Queue [] where
  nil = []
  empty [] = True
  empty _ = False
  pop [] = Nothing
  pop (x:xs) = Just (x,xs)
  insert xs x = x:xs

data Fifo a = Fifo [a] [a]

instance Queue Fifo where
  nil = Fifo [] []
  empty (Fifo [] []) = True
  empty _ = False
  pop (Fifo [] []) = Nothing
  pop (Fifo xs (y:ys)) = Just (y, Fifo xs ys)
  pop (Fifo xs []) = let ys = reverse xs in Just (head ys, Fifo [] (tail ys))
  insert (Fifo xs ys) x = Fifo (x:xs) ys

append :: Fifo a -> [a] -> Fifo a
append (Fifo [] bs) as = Fifo as bs
append (Fifo a1 bs) a2 = Fifo (a2++a1) bs
