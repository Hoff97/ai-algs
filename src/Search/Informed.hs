module Search.Informed where

import Data.PQueue.Min
import qualified Data.Set as Set hiding (filter)

data Node h a = Node { value :: a, parent :: Maybe (Node h a), heur :: h}

instance Eq a => Eq (Node h a) where
  (Node a _ _) == (Node b _ _) = a == b

instance (Eq a, Ord h) => Ord (Node h a) where
  compare (Node _ _ a) (Node _ _ b) = compare a b

insertAll :: Ord a => MinQueue a -> [a] -> MinQueue a
insertAll = foldr insert

path :: Node h a -> [Node h a]
path n@(Node _ Nothing _) = [n]
path n@(Node _ (Just p) _) = n:path p

bfs :: (Ord a, Eq a) => MinQueue (Node Double a) -> Set.Set a -> (a -> [a]) -> (a -> Double) -> Maybe [a]
bfs q v s h = case minView q of
  Just (a,l) -> if heur a == 0
    then Just (value <$> path a)
    else bfs (insertAll l (Prelude.filter (\r -> Set.notMember (value r) v) n)) (Set.insert (value a) v) s h
      where n = (\n -> Node n (Just a) (h n)) <$> s (value a)
  Nothing -> Nothing

newtype AHeur = AHeur (Double,Double)

instance Eq AHeur where
  (AHeur (a,b)) == (AHeur (c,d)) = a+b == c+d

instance Ord AHeur where
  compare (AHeur (a,b)) (AHeur (c,d)) = compare (a+b) (c+d)

fstH (AHeur (a,_)) = a
sumH (AHeur (a,b)) = a+b

aStar :: (Ord a, Eq a) => MinQueue (Node AHeur a) -> Set.Set a -> (a -> [a]) -> (a -> Double) -> Maybe [a]
aStar q v s h = case minView q of
  Just (a,l) -> if sumH (heur a) == 0
    then Just (value <$> path a)
    else aStar (insertAll l (Prelude.filter (\r -> Set.notMember (value r) v) n)) (Set.insert (value a) v) s h
      where
        n = (\n -> Node n (Just a) (AHeur (fstH (heur a) + step (value a) n,h n))) <$> s (value a)
        step a b = abs $ h a - h b
  Nothing -> Nothing
