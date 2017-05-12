module Math.LGS where

import Math.Matrix
import Data.Array

solveLGS :: (Ix i, Num i, Enum i, Num a, Integral i, Fractional a) => Matrix i a -> Array i a -> Array i a
solveLGS a l = let (c,d) = dim a in array (1,d) [(i,solveVar i) | i <- [1..d]]
  where
    solveVar x = det (replaceCol a x l) / da
    da = det a

solveLGSI :: (Ix i, Num i, Enum i, Num a, Integral i, Integral a) => Matrix i a -> Array i a -> Array i a
solveLGSI a l = let (c,d) = dim a in array (1,d) [(i,solveVar i) | i <- [1..d]]
  where
    solveVar x = det (replaceCol a x l) `div` da
    da = det a
