{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Learning.NeuralNet where

import Data.Array
import Math.Matrix
import System.Random
import GHC.Exts (IsList,fromList,toList, Item)

newtype NN a = NN (Array Int (Matrix Int a)) deriving (Show,Read,Functor)

newtype Vec a = Vec (Array Int a) deriving (Show, Functor)

size :: Vec a -> Int
size (Vec a) = snd . bounds $ a

instance Num a => Num (Vec a) where
  (Vec a) + (Vec b) = let (m,n) = bounds a in Vec $ array (m,n) [(i,a!i + b!i) | i <- [m..n]]
  (Vec a) * (Vec b) = let (m,n) = bounds a in Vec $ array (m,n) [(i,a!i * b!i) | i <- [m..n]]
  abs = fmap abs
  signum = fmap signum
  fromInteger x = Vec $ array (1,1) [(1,fromInteger x)]
  negate = fmap negate

type Matr a = Matrix Int a

fromVec :: Vec a -> Matr a
fromVec (Vec vec) = let m = snd . bounds $ vec in matrix (m,1) [(i,1,vec ! i) | i <- [1..m]]

toVec :: Matr a -> Vec a
toVec matr = let m = fst . dim $ matr in Vec (array (1,m) [(i,matr&(i,1)) | i <- [1..m]])

instance IsList (Vec a) where
  type Item (Vec a) = a
  fromList xs = Vec $ array (1,length xs) (zip [1..] xs)
  toList (Vec vec) = [vec!i | i<-[1..snd . bounds $ vec]]

instance IsList (Array Int a) where
  type Item (Array Int a) = a
  fromList xs = array (1,length xs) (zip [1..] xs)
  toList vec = [vec!i | i<-[1..snd . bounds $ vec]]

value :: Num a => NN a -> (a -> a) -> Vec a -> Vec a
value (NN nn) f = go s . fromVec
  where
    (s,e) = bounds nn
    go i vec
      | i>e       = toVec vec
      | otherwise = go (i+1) $ f <$> ((nn!i) * vec)

values :: Num a => NN a -> (a -> a) -> Vec a -> Array Int (Vec (a,a))
values (NN nn) f (Vec vec) = result
  where
    result = array (a,b+1) [(i,if i == a then Vec $ (\a -> (a,a)) <$> vec else layer i) | i <- [a..b+1]]
    layer i = let vals = (nn!(i-1))*fromVec (snd <$> result!(i-1)) in toVec ((\a -> (a,f a)) <$> vals)
    (a,b) = bounds nn

randomNN :: Random a => [(Int,Int)] -> IO (NN a)
randomNN dims = NN <$> x
  where
    x = do
      ls <- sequenceA (randomMatr <$> dims)
      return $ array (1,length ls) (zip [1..] ls)

nn :: a -> [(Int,Int)] -> NN a
nn a dims = NN (array (1,length dims) (zip [1..] (matr a <$> dims)))
