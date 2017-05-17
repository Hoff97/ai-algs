module Util.Queue where

data Queue a = Queue { front :: [a], back :: [a] } deriving Show

instance Eq a => Eq (Queue a) where
  (Queue f1 b1) == (Queue f2 b2) = f1 ++ reverse b1 == f2 ++ reverse b2

emptyQ :: Queue a
emptyQ = Queue [] []

singleQ :: a -> Queue a
singleQ a = Queue [a] []

listQueue :: [a] -> Queue a
listQueue as = Queue (reverse as) []

push :: a -> Queue a -> Queue a
push a (Queue [] []) = Queue [a] []
push a (Queue [] bs) = Queue [] (a:bs)
push a (Queue fs bs) = Queue fs (a:bs)

pop :: Queue a -> Maybe (a,Queue a)
pop (Queue [] []) = Nothing
pop (Queue [] bs) = let (b:b1) = reverse bs in Just (b,Queue b1 [])
pop (Queue [a] bs) = Just (a,Queue (reverse bs) [])
pop (Queue (a:as) bs) = Just (a,Queue as bs)

pushAll :: [a] -> Queue a -> Queue a
pushAll [] q = q
pushAll xs (Queue [] []) = Queue xs []
pushAll xs (Queue as []) = Queue as $ reverse xs
pushAll xs (Queue as bs) = Queue as (reverse xs ++ bs)

popAll :: Queue a -> [a]
popAll (Queue as bs) = as ++ reverse bs
