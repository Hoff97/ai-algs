module Examples.Math where

import Math.Matrix
import Math.LGS
import Math.Galouis
import Data.Array

m1 = fromList (3,3) [1,0,0,0,1,0,0,0,1]

a1 = listArray (1,3) [3,4,5]

t = solveLGS m1 a1
