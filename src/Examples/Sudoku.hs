{-# LANGUAGE FlexibleInstances #-}

module Examples.Sudoku where

import Logic.CSP
import Data.Array
import System.Random

newtype Sudoku = Sudoku (Array (Int,Int) (Maybe Int))

instance Show Sudoku where
  show (Sudoku arr) = unlines rows
    where
      rows = [row i | i <- [0..8]]
      row i = concat [case arr!(i,j) of Just n -> show n; Nothing -> "-"| j <- [0..8]]

instance Read Sudoku where
  readsPrec i s = [(Sudoku $ array ((0,0),(8,8)) [((i,j),(lists !! i)!!j) | i <- [0..8], j <- [0..8]],"")]
    where
      lists :: [[Maybe Int]]
      lists = parseLine <$> lines s
      parseLine x = (\c -> case c of '-' -> Nothing; _ -> Just $ read [c]) <$> x

fullCSP :: CSP (Int,Int) Int
fullCSP = CSP arr constraint neighbour
  where
    arr = array ((0,0),(8,8)) [((i,j),[1..9]) | i <- [0..8], j <- [0..8]]

cspFromSudoku :: Sudoku -> CSP (Int,Int) Int
cspFromSudoku (Sudoku arrI) = CSP arr constraint neighbour
  where
    arr = array ((0,0),(8,8)) [((i,j),getPos i j) | i <- [0..8], j <- [0..8]]
    getPos i j = case arrI!(i,j) of
      Just i -> [i]
      Nothing -> [1..9]

solutionToSudoku :: Array (Int,Int) [Int] -> Sudoku
solutionToSudoku arr = Sudoku $ array ((0,0),(8,8)) [((i,j),Just . head $ arr!(i,j)) | i <- [0..8], j <- [0..8]]

neighbour :: (Int,Int) -> [(Int,Int)]
neighbour (i,j) = [(n,j) | n <- [0..8], n/=i] ++
  [(i,n) | n <- [0..8], n/=j] ++ [(x,y) | let a = i `div` 3, let b = j `div` 3, x <- [a*3..a*3+2], y <- [b*2,b*3+2], x/=i && y/= j]

constraint :: (Int,Int) -> Int -> (Int,Int) -> Int -> Bool
constraint (i,j) x (m,n) y
  | i==m || j==n = x/=y
  | i `div` 3 == m `div` 3 && j `div` 3 == n `div` 3 = x/=y
  | otherwise = True

instance Random (Int,Int) where
  randomR ((x1,y1),(x2,y2)) g = ((x1+a `mod` (y1-x1),x2+b `mod` (y2-x2)),n2)
    where
      (a,n1) = next g
      (b,n2) = next n1
  random g = ((a,b),n2)
    where
      (a,n1) = next g
      (b,n2) = next n1

generateRandomList :: Int -> IO [Int]
generateRandomList 0 = return []
generateRandomList x = do
  a <- randomRIO (1,9)
  as <- generateRandomList (x-1)
  return (a:as)

generateDistinctList :: Int -> [(Int,Int)] -> IO [(Int,Int)]
generateDistinctList 0 l = return l
generateDistinctList x l = do
  a <- randomRIO ((0,0),(8,8))
  if a `notElem` l then generateDistinctList (x-1) (a:l) else generateDistinctList x l
