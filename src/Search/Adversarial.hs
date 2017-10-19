{-# LANGUAGE DeriveFunctor #-}

module Search.Adversarial where

import           Data.List (sortBy)
import           Data.Ord  (comparing)
import Data.Maybe (fromMaybe)
import Util.Tuples
import Debug.Trace


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

minMax :: Heuristic a => GTree a -> Int -> (a -> [a]) -> (a -> Bool) -> Bool -> GTree a
minMax n@(End _ _) _ _ _ _ = n
minMax n@(Next v c h succs) d s end m = Next v numChilds (fromMaybe 0 . fmap heuristic . headS $ sorted) sorted
  where
    comp = if m then heuristic else negate . heuristic
    sorted = sortBy (comparing comp) (first succs')
    numChilds = sum . map childs . first $ succs'
    succs' = foldr walk ([],length succs,d) succs
    walk child (ls,n,d) = let res = minMax child (d `div` n + d `mod` n) s end (not m)
                                in (res:ls,n - 1, d-childs res+childs child)
minMax n@(Cutoff v h) d s end m
  | d <= 0 = n
  | end v = End v h
  | otherwise = Next v numChilds (fromMaybe 0 . fmap heuristic . headS $ sorted) sorted
    where
      comp = if m then heuristic else negate .  heuristic
      children = s v
      sorted = sortBy (comparing comp) (first succs)
      numChilds = sum . map childs . first $ succs
      succs = foldr walk ([],length children,d') children
      d' = d-1
      walk child (ls,n,d) = let res = minMax (Cutoff child 0) (d `div` n) s end (not m)
                                in (res:ls,n - 1, d-childs res-1)

minMaxAB :: Heuristic a => GTree a -> Int -> (a -> [a]) -> (a -> Bool) -> Bool -> Double -> Double -> GTree a
minMaxAB n@(End _ _) _ _ _ _ _ _ = n
minMaxAB n@(Next v c h succs) d s end m alpha beta = Next v numChilds (heuristic . head $ sorted) sorted
  where
    comp = if m then heuristic else negate . heuristic
    sorted = sortBy (comparing comp) (first succs')
    numChilds = sum . map childs . first $ succs'
    succs' = foldr walk ([],length succs,d) succs
    walk child (ls,n,d) = let res = minMaxAB child (d `div` n + d `mod` n) s end (not m) alpha beta
                                in (res:ls,n - 1, d-childs res+childs child)
minMaxAB n@(Cutoff v h) d s end m alpha beta
  | d <= 0 = n
  | alpha > beta = trace "AlphaBeta" n
  | end v = End v h
  | otherwise = Next v numChilds (heuristic . head $ sorted) sorted
    where
      comp = if m then heuristic else negate .  heuristic
      children = s v
      sorted = sortBy (comparing comp) (first succs)
      numChilds = sum . map childs . first $ succs
      succs = foldr walk ([],length children,d',alpha,beta) children
      d' = d-1
      walk child (ls,n,d,a,b) = let res = minMaxAB (Cutoff child 0) (d `div` n) s end (not m) a b
                                in (res:ls,n - 1, d-childs res-1, if m then max a (heuristic res) else a,if not m then min b (heuristic res) else b)


