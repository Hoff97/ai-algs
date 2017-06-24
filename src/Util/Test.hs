{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Util.Test where

import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Debug.Trace (trace)
import System.Random

randList :: Int -> IO [Int]
randList 0 = return []
randList x = do
  a <- randomIO
  as <- randList (x-1)
  return (a:as)

test :: Array Int Int
test = listArray (0,9) [10,25,35,2,45,47,234,8,9,44]

sort :: (Show e, Ord e) => Array Int e -> Array Int e
sort arr = runSTArray $ do
    a <- thaw arr
    (l,r) <- getBounds a
    mergeSort l r a
    return a

mergeSort :: (Show e, Ord e) => Int -> Int -> STArray s Int e -> ST s (STArray s Int e)
mergeSort l r arr
  | l>= r = return arr
  | otherwise = do
    let m = (l+r) `div` 2
    mergeSort l m arr
    mergeSort (m+1) r arr

    copy <- newArray_ (l,r)
    merge l l m (m+1) r arr copy
    forM_ [l..r] $ \i -> do
      x <- readArray copy i
      writeArray arr i x

    return arr

merge :: Ord e => Int -> Int -> Int -> Int -> Int -> STArray s Int e -> STArray s Int e -> ST s ()
merge i l m k r res copy
  | i > r = return ()
  | l > m = forM_ [k..r] $ \j -> do
      x <- readArray res j
      writeArray copy j x
  | k > r = forM_ [l..m] $ \j -> do
      x <- readArray res j
      writeArray copy (i+j-l) x
  | otherwise = do
    x <- readArray res l
    y <- readArray res k
    if x<y then do
      writeArray copy i x
      merge (i+1) (l+1) m k r res copy
    else do
      writeArray copy i y
      merge (i+1) l m (k+1) r res copy
