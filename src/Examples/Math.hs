{-# LANGUAGE DataKinds #-}

module Examples.Math where

import Math.Matrix
import Math.LGS
import Math.Galois
import Data.Array

m1 :: Integral a => Matrix Integer (GFs 2 a)
m1 = fromList (3,3) [1,0,0,0,1,0,0,0,1]

a1 :: Num a => Array Integer a
a1 = listArray (1,3) [3,4,5]

t = solveLGSI m1 a1
