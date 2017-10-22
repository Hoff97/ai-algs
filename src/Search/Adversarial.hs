{-# LANGUAGE DeriveFunctor #-}

module Search.Adversarial where

import           Data.List (sortBy)
import           Data.Ord  (comparing)
import Data.Maybe (fromMaybe)
import Util.Tuples
import Debug.Trace
import Util.Memoize
import Data.Foldable (foldrM)


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



minMaxAB' :: (Heuristic a, Ord a) => (a -> [a]) -> (a -> Bool) -> (GTree a,Int,Bool,Double,Double) -> Memo a (GTree a) (GTree a)
minMaxAB' succ end t = memoizeChange' trans h t
  where
    trans = value . first
    h (n,d,m,alpha,beta) (Just x) = h (n,d,m,alpha,beta) Nothing
    h (n@(End _ _),d,m,alpha,beta) Nothing = return n
    h (n@(Cutoff v h),d,m,alpha,beta) Nothing
      | d <= 0 = return n
      | alpha > beta = return n
      | end v = return $ End v h
      | otherwise = do
          let children = succ v
          let walk child (ls,n,d,a,b) = do
                res <- minMaxAB' succ end (Cutoff child 0,d `div` n,not m, a, b)
                return (res:ls,n - 1, d - childs res - 1, if m then max a (heuristic res) else a,if not m then min b (heuristic res) else b)
          s <- foldrM walk ([], length children,d-1,alpha,beta) children
          let numChilds = sum . map childs . first $ s
          let comp = if m then heuristic else negate .  heuristic
          let sorted = sortBy (comparing comp) (first s)
          return $ Next v numChilds (heuristic . head $ sorted) sorted
    h (n@(Next v c h succs),d,m,alpha,beta) Nothing
      | d <= 0 = return n
      | alpha > beta = return n
      | otherwise = do
      let comp = if m then heuristic else negate . heuristic
      let walk child (ls,n,d, a, b) = do
            res <- minMaxAB' succ end (child,d `div` n,not m, a, b)
            return (res:ls,n - 1, d-childs res+childs child,if m then max a (heuristic res) else a,if not m then min b (heuristic res) else b)
      s <- foldrM walk ([],length succs,d,alpha,beta) succs
      let sorted = sortBy (comparing comp) (first s)
      let numChilds = sum . map childs . first $ s
      return $ Next v numChilds (heuristic . head $ sorted) sorted

