{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Math.Matrix
import Learning.NeuralNet
import Data.Array
import Learning.Backpropagation
import System.Random

main :: IO ()
main = do
  print "Hi"

rot :: Matr Double
rot = fromList (2,2) [0,-1,1,0]

genTuples :: Int -> IO [(Vec Double,Vec Double)]
genTuples 0 = return []
genTuples i = do
  a <- randomIO
  b <- randomIO
  let input :: Vec Double = [a,b]
  let output = toVec (rot*fromVec input)
  ls <- genTuples (i-1)
  return $ (input,output):ls
