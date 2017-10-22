module Examples.Tic where

import           Search.Adversarial

import           Data.Array

import           Data.List          (nub)

import           Util.Tuples

data Field = Empty | White | Black deriving (Eq,Ord)

instance Show Field where
  show Empty = " "
  show White = "O"
  show Black = "X"

instance Heuristic Field where
  heuristic Empty = 0
  heuristic White = 1
  heuristic Black = -1

data TicTac = Tic (Array (Int,Int) Field) Bool deriving (Eq,Ord)

white :: TicTac -> Bool
white (Tic _ b) = b

instance Show TicTac where
  show (Tic a _) = "\nx---x\n" ++
                 "|" ++ show (a!(0,0)) ++ show (a!(0,1)) ++ show (a!(0,2)) ++ "|\n" ++
                 "|" ++ show (a!(1,0)) ++ show (a!(1,1)) ++ show (a!(1,2)) ++ "|\n" ++
                 "|" ++ show (a!(2,0)) ++ show (a!(2,1)) ++ show (a!(2,2)) ++ "|\n" ++
                 "x---x"

instance Heuristic TicTac where
  heuristic g@(Tic a _)
    | any (all (== White)) r = 100
    | any (all (== Black)) r = -100
    | otherwise = sum . fmap heuristic $ a
      where
        r = rows g

start :: TicTac
start = Tic (listArray ((0,0),(2,2)) $ replicate 9 Empty) True

rows :: TicTac -> [[Field]]
rows (Tic a _) = [[a!(0,0),a!(0,1),a!(0,2)], [a!(1,0),a!(1,1),a!(1,2)], [a!(2,0),a!(2,1),a!(2,2)],
               [a!(0,0),a!(1,0),a!(2,0)],[a!(0,1),a!(1,1),a!(2,1)],[a!(0,2),a!(1,2),a!(2,2)],
               [a!(0,0),a!(1,1),a!(2,2)], [a!(0,2),a!(1,1),a!(2,0)]]

place :: TicTac -> Field -> (Int,Int) -> TicTac
place (Tic a n) p i
  | a!i == Empty = Tic (a//[(i,p)]) (not n)
  | otherwise = Tic a n

full :: TicTac -> Bool
full (Tic a _) = all (/= Empty) a

end :: TicTac -> Bool
end t = abs (heuristic t) == 100 || full t

next :: TicTac -> [TicTac]
next a = nub . filter (/= a) $ (place a (if white a then White else Black) <$> [(i,j) | i <- [0..2], j <- [0..2]])
