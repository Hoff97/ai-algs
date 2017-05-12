{-# LANGUAGE TupleSections #-}

module Logic.CSP where

import Data.Array
import qualified Data.Map.Strict as M
import Control.Arrow (second)
import Data.List (sortBy)
import Data.Ord (comparing)
import Util.Queue

data CSP i a = CSP { domain :: Array i [a], constraints :: i -> a -> i -> a -> Bool, neighbours :: i -> [i] }

solve :: Ix i => CSP i a -> [Array i [a]]
solve c@(CSP v _ _) = solveCSP c (sortBy (comparing (\i -> length $ v!i)) . range $ bounds v)

solveCSP :: Ix i => CSP i a -> [i] -> [Array i [a]]
solveCSP (CSP a c n) [] = [a]
solveCSP (CSP a c n) (i:is) = (a!i) >>= recSolve
  where
    updateVals v = filter . c i v
    recSolve v = case updated v of
      Just u  -> solveCSP (CSP u c n) (sortBy (comparing (\j -> length $ u!j)) is)
      Nothing -> []
    updated v = ac3 (listQueue ((i,) <$> n i)) is $ CSP (a // [(i,[v])]) c n

ac3 :: Ix i => Queue (i,i) -> [i] -> CSP i a -> Maybe (Array i [a])
ac3 q is (CSP vs c n) = case pop q of
  Just ((i,j),q1) -> case revise (i,vs!i) (j,vs!j) c of
    Just []   -> Nothing
    Just as   -> ac3 (pushAll ((j,) <$> filter (`elem` is) (n j)) q1) is (CSP (vs//[(j,as)]) c n)
    Nothing   -> ac3 q1 is (CSP vs c n)
  Nothing     -> Just vs

revise :: (i,[a]) -> (i,[a]) -> (i -> a -> i -> a -> Bool) -> Maybe [a]
revise (i,vs) (j,ws) c = case foldr (\v (l,vals) -> if any (c j v i) vs then (l,v:vals) else (True,vals)) (False,[]) ws of
  (True,vs) -> Just vs
  (False,_) -> Nothing

--TODO: AC-3
