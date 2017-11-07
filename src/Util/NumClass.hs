module Util.NumClass where

instance (Num a, Num b) => Num (a,b) where
  (a,b) + (c,d) = (a+c,b+d)
  (a,b) * (c,d) = (a*c,b*d)
  abs (a,b) = (abs a,abs b)
  signum (a,b) = (signum a, signum b)
  fromInteger i = (fromInteger i,fromInteger i)
  negate (a,b) = (negate a, negate b)
