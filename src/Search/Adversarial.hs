{-# LANGUAGE DeriveFunctor #-}

module Search.Adversarial where

import           Data.List (sortBy)
import           Data.Ord  (comparing)
import Data.Maybe (fromMaybe)


class Heuristic a where
  heuristic :: a -> Double
  inverted :: a -> Double
  inverted = negate . heuristic

data GTree a = End a Double | Cutoff a Double | Next a Int Double [GTree a] deriving (Show, Eq, Functor)

childs :: GTree a -> Int
childs (Next _ i _  _) = i+1
childs _               = 1

value :: GTree a -> a
value (Next n _ _ _) = n
value (End n _)      = n
value (Cutoff n _)   = n

instance Heuristic a => Heuristic (GTree a) where
  heuristic (End _ h)      = h
  heuristic (Cutoff _ h)   = h
  heuristic (Next _ _ h _) = h

headS :: [a] -> Maybe a
headS [] = Nothing
headS (x:xs) = Just x

minMax :: Heuristic a => a -> Int -> (a -> [a]) -> (a -> Double) -> (a -> Bool) -> Bool -> GTree a
minMax n d s heur end m
  | end n = End n (heur n)
  | d <= 0 = Cutoff n (heur n)
  | otherwise = Next n numChilds (fromMaybe 0 . fmap heuristic . headS $ sorted) sorted
    where
      comp = if m then heuristic else negate .  heuristic
      sorted = sortBy (comparing comp) succs
      numChilds = sum . map childs $ succs
      succs = (\n' -> minMax n' (d-1) s heur end (not m)) <$> s n

minMax' :: Heuristic a => GTree a -> Int -> (a -> [a]) -> (a -> Double) -> (a -> Bool) -> Bool -> GTree a
minMax' n@(End _ _) _ _ _ _ _ = n
minMax' n@(Next v c h succs) d s heur end m = undefined
minMax' n@(Cutoff v h) d s heur end m
  | d <= 0 = n
  | otherwise = Next v numChilds (fromMaybe 0 . fmap heuristic . headS $ sorted) sorted
    where
      comp = if m then heuristic else negate .  heuristic
      children = s v
      sorted = sortBy (comparing comp) (first succs)
      numChilds = d-third succs
      succs = foldr walk ([],length children,d) children
      d' = d-1
      walk child (ls,n,d) = let res = minMax' (Cutoff child 0) (d' `div` n + d' `mod` n) s heur end (not m)
                                in (res:ls,n - 1, d-childs res)

first :: (a,b,c) -> a
first (a,_,_) = a

second :: (a,b,c) -> b
second (_,b,_) = b

third :: (a,b,c) -> c
third (_,_,c) = c
