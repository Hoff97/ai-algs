module Search.Online where

import Data.List (minimumBy)
import Data.Function (on)

lrta :: a -> (a -> [a]) -> (a -> Double) -> Maybe (Double,a)
lrta n s h
  | h n == 0  = Just (0,n)
  | otherwise = case filter ((/= -2) . h) $ s n of
    []  -> Nothing
    [x] -> Just (-2,x)
    xs  -> let m = minimumBy (compare `on` h) xs in Just (h n + 1,m)
