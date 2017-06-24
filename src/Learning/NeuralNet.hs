{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

module Learning.NeuralNet where

import Data.Array
import Math.Matrix
import System.Random
import GHC.Exts (IsList,fromList,toList, Item)

newtype NN a = NN (Array Int (Matrix Int a)) deriving Show

newtype Vec a = Vec (Array Int a) deriving Show

type Matr a = Matrix Int a

fromVec :: Vec a -> Matr a
fromVec (Vec vec) = let m = snd . bounds $ vec in matrix (m,1) [(i,1,vec ! i) | i <- [1..m]]

toVec :: Matr a -> Vec a
toVec matr = let m = fst . dim $ matr in Vec (array (1,m) [(i,matr&(i,1)) | i <- [1..m]])

instance IsList (Vec a) where
  type Item (Vec a) = a
  fromList xs = Vec $ array (1,length xs) (zip [1..] xs)
  toList (Vec vec) = [vec!i | i<-[1..snd . bounds $ vec]]

value :: Num a => NN a -> (a -> a) -> Vec a -> Vec a
value (NN nn) f = go s . fromVec
  where
    (s,e) = bounds nn
    go i vec
      | i>e       = toVec vec
      | otherwise = go (i+1) $ f <$> (vec * (nn!i))

randomNN :: Random a => [(Int,Int)] -> IO (NN a)
randomNN dims = NN <$> x
  where
    x = do
      ls <- sequenceA (randomMatr <$> dims)
      return $ array (1,length ls) (zip [1..] ls)
