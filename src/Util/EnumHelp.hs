{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.EnumHelp where

instance (Eq b, Bounded b, Enum b, Enum a, Bounded a, Eq a) => Enum (a,b) where
  succ (a,b)
    | b == maxBound = (succ a,minBound)
    | otherwise = (a,succ b)
  fromEnum (a,b) = fromEnum a*sizeB+fromEnum b
    where
      sizeB = fromEnum (maxBound @ b) - fromEnum (minBound @ b)
  toEnum n = (toEnum $ n `div` sizeB, toEnum $ n `mod` sizeB)
    where
      sizeB = fromEnum (maxBound @ b) - fromEnum (minBound @ b)

  enumFrom (a,b) = walk (a,b)
    where
      walk (a',b')
        | b' == maxBound && a' == maxBound = [(a',b')]
        | b' == maxBound = (a',b'):walk (succ a', b)
        | otherwise = (a',b'):walk (a',succ b')

  enumFromTo (a,b) (c,d) = walk (a,b)
    where
      walk (a',b')
        | b' == d && a' == c = [(a',b')]
        | b' == d = (a',b'):walk (succ a', b)
        | otherwise = (a',b'):walk (a',succ b')
