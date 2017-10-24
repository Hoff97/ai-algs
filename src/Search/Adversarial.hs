{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module Search.Adversarial where

import           Data.List (sortBy)
import           Data.Ord  (comparing)
import Data.Maybe (fromMaybe)
import Util.Tuples
import Debug.Trace
import Util.Memoize


import Data.Foldable (foldrM,foldlM)
import Data.Hashable
import Data.PQueue.Prio.Max as PQ


class Heuristic a where
  heuristic :: a -> Double
  inverted :: a -> Double
  inverted = negate . heuristic

data GTreeT f a = End a Double | Cutoff a Double | Next a Int Double Bool (f (GTreeT f a))
type GTree a = GTreeT [] a

instance (Show a, Show (f (GTreeT f a))) => Show (GTreeT f a) where
  show (End a _) = "End " ++ show a
  show (Cutoff a _) = "Cutoff " ++ show a
  show (Next a _ _ _ f) = "Next (" ++ show a ++ ") (" ++ show f ++ ")"

--data GTree a = End a Double | Cutoff a Double | Next a Int Double Bool [GTree a] deriving (Show, Eq, Functor)

childs :: GTreeT f a -> Int
childs (Next _ i _ _ _) = i+1
childs _               = 1

value :: GTreeT f a -> a
value (Next n _ _ _ _) = n
value (End n _)      = n
value (Cutoff n _)   = n

done :: GTreeT f a -> Bool
done (End _ _) = True
done (Cutoff _ _) = False
done (Next _ _ _ d _) = d

move :: GTree a -> a
move (End a _) = a
move (Cutoff a _) = a
move (Next _ _ _ _ (x:xs)) = value x

instance Heuristic a => Heuristic (GTreeT f a) where
  heuristic (End _ h)      = h
  heuristic (Cutoff _ h)   = h
  heuristic (Next _ _ h _ _) = h

headS :: [a] -> Maybe a
headS [] = Nothing
headS (x:xs) = Just x

minMax :: Heuristic a => GTree a -> Int -> (a -> [a]) -> (a -> Bool) -> Bool -> GTree a
minMax n@(End _ _) _ _ _ _ = n
minMax n@(Next v c h done' succs) d s end m = Next v numChilds (fromMaybe 0 . fmap heuristic . headS $ sorted) done' sorted
  where
    comp = if m then heuristic else negate . heuristic
    sorted = sortBy (comparing comp) (first succs')
    numChilds = sum . fmap childs . first $ succs'
    done' = all done . first $ succs'
    succs' = foldr walk ([],length succs,d) succs
    walk child (ls,n,d) = let res = minMax child (d `div` n + d `mod` n) s end (not m)
                                in (res:ls,n - 1, d-childs res+childs child)
minMax n@(Cutoff v h) d s end m
  | d <= 0 = n
  | end v = End v h
  | otherwise = Next v numChilds (fromMaybe 0 . fmap heuristic . headS $ sorted) done' sorted
    where
      comp = if m then heuristic else negate .  heuristic
      children = s v
      sorted = sortBy (comparing comp) (first succs)
      numChilds = sum . fmap childs . first $ succs
      succs = foldr walk ([],length children,d') children
      d' = d-1
      done' = all done . first $ succs
      walk child (ls,n,d) = let res = minMax (Cutoff child 0) (d `div` n) s end (not m)
                                in (res:ls,n - 1, d-childs res-1)

minMaxAB :: Heuristic a => GTree a -> Int -> (a -> [a]) -> (a -> Bool) -> Bool -> Double -> Double -> GTree a
minMaxAB n@(End _ _) _ _ _ _ _ _ = n
minMaxAB n@(Next v c h d' succs) d s end m alpha beta = Next v numChilds (heuristic . head $ sorted) done' sorted
  where
    comp = if m then heuristic else negate . heuristic
    sorted = sortBy (comparing comp) (first succs')
    numChilds = sum . fmap childs . first $ succs'
    succs' = foldr walk ([],length succs,d) succs
    done' = all done . first $ succs'
    walk child (ls,n,d) = let res = minMaxAB child (d `div` n + d `mod` n) s end (not m) alpha beta
                                in (res:ls,n - 1, d-childs res+childs child)
minMaxAB n@(Cutoff v h) d s end m alpha beta
  | d <= 0 = n
  | alpha > beta = Cutoff v h
  | end v = End v h
  | otherwise = Next v numChilds (heuristic . head $ sorted) done' sorted
    where
      comp = if m then heuristic else negate .  heuristic
      children = s v
      sorted = sortBy (comparing comp) (first succs)
      numChilds = sum . fmap childs . first $ succs
      succs = foldr walk ([],length children,d',alpha,beta) children
      d' = d-1
      done' = all done . first $ succs
      walk child (ls,n,d,a,b) = let res = minMaxAB (Cutoff child 0) (d `div` n) s end (not m) a b
                                in (res:ls,n - 1, d-childs res-1, if m then max a (heuristic res) else a,if not m then min b (heuristic res) else b)


minMaxAB' :: (Heuristic a, Hashable a, Eq a) => (a -> [a]) -> (a -> Bool) -> (GTree a,Int,Bool,Double,Double) -> Memo a (GTree a) (GTree a)
minMaxAB' succ end t = memoizeChange' trans h t
  where
    trans = value . first
    h (n,d,m,alpha,beta) (Just x) = h (x,d,m,alpha,beta) Nothing
    h (n@(End _ _),d,m,alpha,beta) Nothing = return n
    h (n@(Cutoff v h),d,m,alpha,beta) Nothing
      | d <= 0 = return $ Cutoff v (heuristic v)
      | alpha > beta = return $ Cutoff v (heuristic v)
      | end v = return $ End v (heuristic v)
      | otherwise = do
          let children = succ v
          let walk child (ls,n,d,a,b) = do
                res <- minMaxAB' succ end (Cutoff child 0,d `div` n,not m, a, b)
                return (res:ls,n - 1, d - childs res - 1, if m then max a (heuristic res) else a,if not m then min b (heuristic res) else b)
          s <- foldlM (flip walk) ([], length children,d-1,alpha,beta) children
          let numChilds = sum . fmap childs . first $ s
          let comp = if m then negate . heuristic else heuristic
          let sorted = sortBy (comparing comp) (first s)
          let done' = all done . first $ s
          return $ Next v numChilds (heuristic . head $ sorted) done' sorted
    h (n@(Next v c h d' succs),d,m,alpha,beta) Nothing
      | d <= 0 = return n
      | alpha > beta = return n
      | d' = return n
      | otherwise = do
      let comp = if m then negate . heuristic else heuristic
      let walk child (ls,n,d, a, b) = do
            res <- minMaxAB' succ end (child,d `div` n,not m, a, b)
            return (res:ls,n - 1, d-childs res+childs child,if m then max a (heuristic res) else a,if not m then min b (heuristic res) else b)
      s <- foldlM (flip walk) ([],length succs,d,alpha,beta) succs
      let sorted = sortBy (comparing comp) (first s)
      let numChilds = sum . fmap childs . first $ s
      let done' = all done . first $ s
      return $ Next v numChilds (heuristic . head $ sorted) done' sorted



type GTreeP a = GTreeT (MaxPQueue Double) a

minMaxABPrio :: (Heuristic a, Hashable a, Eq a) => (a -> [a]) -> (a -> Bool) -> (GTreeP a,Int,Bool,Double,Double) -> Memo a (GTreeP a) (GTreeP a)
minMaxABPrio succ end t = memoizeChange' trans h t
  where
    trans = value . first
    h (n,d,m,alpha,beta) (Just x) = h (x,d,m,alpha,beta) Nothing
    h (n@(End _ _),d,m,alpha,beta) Nothing = return n
    h (n@(Cutoff v h),d,m,alpha,beta) Nothing
      | d <= 0 = return $ Cutoff v (heuristic v)
      | alpha > beta = return $ Cutoff v (heuristic v)
      | end v = return $ End v (heuristic v)
      | otherwise = do
          let children = succ v
          let heur' = if m then heuristic else inverted
          let walk child (ls,n,d,a,b,dn,chs) = do
                res <- minMaxABPrio succ end (Cutoff child 0,d `div` n,not m, a, b)
                return (PQ.insert (heur' res) res ls,n - 1, d - childs res - 1, if m then max a (heuristic res) else a,if not m then min b (heuristic res) else b,dn&&done res,chs+childs res)
          (els,n,d,a,b,dn,chs) <- foldlM (flip walk) (PQ.empty, length children,d-1,alpha,beta,True,0) children
          return $ Next v chs (first . findMax $ els) dn els
    h (n@(Next v c h d' succs),d,m,alpha,beta) Nothing
      | d <= 0 = return n
      | alpha > beta = return n
      | d' = return n
      | otherwise = do
      let heur' = if m then heuristic else inverted
      let walk child (ls,n,d, a, b, dn, chs) = do
            res <- minMaxABPrio succ end (child,d `div` n,not m, a, b)
            return (PQ.insert (heur' res) res ls,n - 1, d-childs res+childs child,if m then max a (heuristic res) else a,if not m then min b (heuristic res) else b, dn&&done res,chs+childs res)
      (els,n,d,a,b,dn,chs) <- foldrM' walk (PQ.empty,length succs,d,alpha,beta,True,0) succs
      return $ Next v chs (first . findMax $ els) dn els

foldrM' :: Monad m => (a -> b -> m b) -> b -> (MaxPQueue Double a) -> m b
foldrM' f b ls = foldrWithKey app (return b) ls
  where
    app _ a mb = do
      !b <- mb
      f a b

foldlM' :: Monad m => (b -> a -> m b) -> b -> (MaxPQueue Double a) -> m b
foldlM' f b ls = foldlWithKey app (return b) ls
  where
    app mb _ a = do
      !b <- mb
      f b a
