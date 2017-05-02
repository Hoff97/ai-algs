{-# LANGUAGE ScopedTypeVariables #-}

module Search.Continuous where

import System.Random
import Debug.Trace

hillClimb :: (Show a, Ord a) => a -> (a -> [a]) -> a
hillClimb a g = if a > maxN then a else traceShow maxN $ hillClimb maxN g
  where
    maxN = maximum $ g a

e = exp 1

--Simulated annealing
simAnn :: RandomGen g => g -> (a -> Double) -> (Int -> Double) -> (g -> a -> (a,g)) -> a -> a
simAnn g v s n i = walk g v s n i 0
  where
    walk g v s n i t
      | s t == 0  = i
      | otherwise = let
        (next,gn1) = n g i
        (rD :: Double, gn) = random gn1
        dV = v next - v i in
        if dV > 0 then walk gn1 v s n next (t+1)
        else if rD < (e**(dV/s t)) then walk gn v s n next (t+1)
        else walk gn v s n i (t+1)
