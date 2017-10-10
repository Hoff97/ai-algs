{-# LANGUAGE DeriveFunctor #-}

module Search.Adversarial where

import           Data.List (sortBy)
import           Data.Ord  (comparing)


class Heuristic a where
  heuristic :: a -> Double
  inverted :: a -> Double
  inverted = negate . heuristic

data GTree a = End a Double | Cutoff a Double | Next a Double [GTree a] deriving (Show, Eq, Functor)

instance Heuristic a => Heuristic (GTree a) where
  heuristic (End _ h)    = h
  heuristic (Cutoff _ h) = h
  heuristic (Next _ h _) = h

minMax :: Heuristic a => a -> Int -> (a -> [a]) -> (a -> Double) -> (a -> Bool) -> Bool -> GTree a
minMax n d s heur end m
  | end n = End n (heur n)
  | d <= 0 = Cutoff n (heur n)
  | otherwise = Next n (heuristic . head $ sorted) sorted
    where
      comp = if m then heuristic else negate .  heuristic
      sorted = sortBy (comparing comp) succs
      succs = (\n' -> minMax n' (d-1) s heur end (not m)) <$> s n
