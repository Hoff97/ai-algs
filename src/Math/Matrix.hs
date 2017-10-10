{-# LANGUAGE TupleSections #-}

module Math.Matrix where

import Data.Array
import System.Random

newtype Matrix i a = Matrix (Array (i,i) a) deriving (Show, Read)

rows :: (Enum i, Ix i, Num i) => Matrix i a -> [[a]]
rows b@(Matrix a) = let (m,n) = dim b in [(\j -> a!(i,j)) <$> [1..n] | i <- [1..m]]

matrix :: (Ix i, Num i) => (i,i) -> [(i,i,a)] -> Matrix i a
matrix (i,j) l = Matrix (array ((1,1),(i,j)) ((\(i,j,a) -> ((i,j),a)) <$> l))

fromList :: (Num i, Ix i) => (i,i) -> [a] -> Matrix i a
fromList (i,j) l = Matrix $ listArray ((1,1),(i,j)) l

arr :: Matrix i a -> Array (i,i) a
arr (Matrix a) = a

dim :: Matrix i a -> (i,i)
dim = snd . bounds . arr

(&) :: Ix i => Matrix i a -> (i,i) -> a
a & i = arr a!i

instance Functor (Matrix i) where
  fmap f (Matrix a) = Matrix (f <$> a)

instance (Enum i, Num i, Ix i, Num a) => Num (Matrix i a) where
  a + b = matrix (m,n) [(i,j,a&(i,j) + b&(i,j)) | i <- [1..m], j <- [1..n]]
    where
       ((m,n),(n1,o)) = (dim a,dim b)
  a * b = matrix (m,o) [(i,k,sum ((\j -> a&(i,j)*b&(j,k)) <$> [1..n])) | i <- [1..m], k <- [1..o]]
    where
      ((m,n),(n1,o)) = (dim a,dim b)
  abs = fmap abs
  signum = fmap signum
  fromInteger x = matrix (1,1) [(1,1,fromInteger x)]
  negate = fmap negate

(&^) :: (Enum i,Num i, Ix i, Num a) => Matrix i a -> Int -> Matrix i a
a &^ 1 = a
a &^ n = a*(a&^(n-1))

minor :: (Num i, Ix i, Enum i) => Matrix i a -> (i,i) -> Matrix i a
minor a (m,n) = let (c,d) = dim a in
  matrix (c-1,d-1) [(if i<m then i else i-1,if j<n then j else j-1,a&(i,j)) | i <- [1..c], j <- [1..d], i/=m&&j/=n]

det :: (Num i, Eq i, Ix i, Num a, Integral i) => Matrix i a -> a
det a
  | c==1 && d==1 = a&(1,1)
  | otherwise = sum ((\i -> (-1)^(1+i) * a&(i,1) * det (minor a (i,1))) <$> [1..c])
  where (c,d) = dim a

replaceCol :: (Enum i, Num i, Ix i) => Matrix i a -> i -> Array i a -> Matrix i a
replaceCol a ci col = let (c,d) = dim a in matrix (c,d) [(i,j,if j==ci then col!i else a&(i,j)) | i <- [1..c], j <- [1..d]]

randomMatr :: Random a => (Int,Int) -> IO (Matrix Int a)
randomMatr (m,n) = matrix (m,n) <$> sequenceA [(i,j,) <$> randomIO | i <- [1..m], j <- [1..n]]

matr :: a -> (Int,Int) -> Matrix Int a
matr a (m,n) = matrix (m,n) [(i,j,a) | i <- [1..m], j <- [1..n]]
