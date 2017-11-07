{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Examples.GameQueens where

import           Data.Array
import           Data.Foldable
import           Data.Hashable
import           Debug.Trace
import           GHC.Generics
import           Search.Adversarial
import           Search.Adversarial
import           Util.EnumHelp
import           Util.Memoize

data QPiece = Stone | Queen deriving (Eq, Generic)

instance Hashable QPiece

data Field a = Black a | White a | Empty deriving (Eq, Generic)

instance Hashable a => Hashable (Field a)

instance Show (Field QPiece) where
  show (Black Stone) = "x"
  show (Black Queen) = "X"
  show (White Stone) = "o"
  show (White Queen) = "O"
  show Empty         = " "

black :: Field t -> Bool
black (Black _) = True
black _         = False

white :: Field t -> Bool
white (White _) = True
white _         = False

empty :: Field t -> Bool
empty Empty = True
empty _     = False

queen :: Field QPiece -> Bool
queen (White Queen) = True
queen (Black Queen) = True
queen _             = False

color :: Field QPiece -> Field ()
color (Black _) = Black ()
color (White _) = White ()
color Empty     = Empty

data Board a = Board (Array (Int,Int) a) Bool deriving (Eq, Generic)

instance Hashable a => Hashable (Board a) where
  hashWithSalt salt (Board a b) = hashWithSalt salt (elems a,b)

instance Show QBoard where
  show (Board a _) = "+-+-+-+-+-+-+-+-+\n" ++ concat rows ++ "+-+-+-+-+-+-+-+-+"
    where
      rows = map (\(i,j) -> "|" ++ (if i `mod` 2 == 0 then " |" else "") ++ show (a!(i,j)) ++ (if i `mod` 2 == 1 then "| " else "") ++ (if j==3 then "|\n" else "")) [(0,0)..(7,3)]

instance Heuristic QPiece where
  heuristic Stone = 1
  heuristic Queen = 5

instance Heuristic a => Heuristic (Field a) where
  heuristic (Black p) = inverted p
  heuristic (White p) = heuristic p
  heuristic Empty     = 0

instance Heuristic a => Heuristic (Board a) where
  heuristic (Board a _) = sum . fmap heuristic $ a

type QField = Field QPiece
type QBoard = Board QField

start :: QBoard
start = Board (listArray ((0,0),(7,3)) $ replicate 12 (White Stone) ++ replicate 8 Empty ++ replicate 12 (Black Stone)) True

w = White Stone
q = White Queen
b = Black Stone
e = Empty

end :: QBoard -> Bool
end (Board a _) = onlyBlack || onlyWhite
  where
    onlyBlack = foldr (\a b -> b && (black a || empty a)) True a
    onlyWhite = foldr (\a b -> b && (white a || empty a)) True a

next :: QBoard -> [QBoard]
next (Board a p) = if not (null beat) then map (\a -> Board a (not p)) beat else map (\a -> Board a (not p)) simpleMove
  where
    beat = concat $ map (checkBeatMove a) [(0,0)..(7,3)]
    checkBeatMove a (i,j)
      | not (inBounds (i,j)) = []
      | a!(i,j) == Empty = []
      | black (a!(i,j)) && p = []
      | white (a!(i,j)) && not p = []
      | queen (a!(i,j)) = [lu,ld,ru,rd] >>= beatQueen a (i,j) (i,j)
      | otherwise = (if p then [lu,ru] else [ld,rd]) >>= beatTo (i,j) a
    beatTo (i,j) a f
      | j'<0 || j'>3 || i'<0 || i'>7 = []
      | a!(i',j') == Empty && a!(c,d) /= Empty && color (a!(c,d)) /= color (a!(i,j)) = [a // [((i,j),Empty),((c,d),Empty),((i',j'),piece)]] >>= maybeBeat (i',j')
      | otherwise = []
        where
          piece = if p && i'==7 || not p && i'==0 then (if p then White Queen else Black Queen) else a!(i,j)
          maybeBeat (i,j) a = case checkBeatMove a (i,j) of
            [] -> [a]
            ls -> ls
          (c,d) = f (i,j)
          (i',j') = f (c,d)
    beatQueen a (i,j) (s,t) f
      | s<0 || s>7 || t<0 || t>3 = []
      | i == s && j == t = beatQueen a (i,j) (f (i,j)) f
      | (a!(s,t)) == Empty = beatQueen a (i,j) (f (s,t)) f
      | (a!(s,t)) /= Empty
        && color (a!(i,j)) /= color (a!(s,t)) = let n = f (s,t) in
          if inBounds n && (a!n == Empty) then
            [a//[((i,j),Empty),((s,t),Empty),(n,a!(i,j))]] >>= maybeBeatQueen n
          else []
      | otherwise = []
    maybeBeatQueen i a = case [lu,ld,ru,rd] >>= beatQueen a i i of
      [] -> [a]
      ls -> ls

    simpleMove = concat $ map checkMove [(0,0)..(7,3)]
    checkMove (i,j)
      | not (inBounds (i,j)) = []
      | a!(i,j) == Empty = []
      | black (a!(i,j)) && p = []
      | white (a!(i,j)) && not p = []
      | queen (a!(i,j)) = [lu,ld,ru,rd] >>= walkQueen (i,j) (i,j)
      | otherwise = (if p then [lu (i,j), ru (i,j)] else [ld (i,j), rd (i,j)]) >>= moveTo (i,j)
    moveTo (i,j) (i',j')
      | not (inBounds (i',j')) = []
      | a!(i',j') == Empty && (p && i'==7 || not p && i' == 0) = [a // [((i,j),Empty),((i',j'),if p then White Queen else Black Queen)]]
      | a!(i',j') == Empty = [a // [((i,j),Empty),((i',j'),a!(i,j))]]
      | otherwise = []
    walkQueen (i,j) (s,t) f
      | not (inBounds (s,t)) = []
      | i == s && j == t = walkQueen (i,j) (f (i,j)) f
      | (a!(s,t)) /= Empty = []
      | otherwise = (a//[((i,j),Empty),((s,t),a!(i,j))]):walkQueen (i,j) (f (s,t)) f

inBounds (i,j) = i>=0 && i<=7 && j>=0 && j<=3

lu :: (Int,Int) -> (Int,Int)
lu (i,j) = (i+1,j+((i+1) `mod` 2)-1)

ld :: (Int,Int) -> (Int,Int)
ld (i,j) = (i-1,j+((i+1) `mod` 2)-1)

ru :: (Int,Int) -> (Int,Int)
ru (i,j) = (i+1,j+((i+1) `mod` 2))

rd :: (Int,Int) -> (Int,Int)
rd (i,j) = (i-1,j+((i+1) `mod` 2))

heur :: QBoard -> Double
heur b@(Board a _)
  | onlyBlack = -10000
  | onlyWhite = 10000
  | next b == [] = 0
  | otherwise = foldr ((+) . heuristic) 0 a
  where
    onlyBlack = foldr (\a b -> b && (black a || empty a)) True a
    onlyWhite = foldr (\a b -> b && (white a || empty a)) True a

playAgainst :: Int -> QBoard -> (GTreeP QBoard) -> Bool -> Bool -> IO ()
playAgainst d f tree b p
  | end f = print "Game has ended" >> return ()
  | not p = do
      let mv = runMemo $ minMaxABPrio next end (tree,d,b,-100000,100000)
      printInfo mv
      playAgainst d (move mv) (moveTree mv) b (not p)
  | otherwise = do
      let n = zip [1..] $ next f
      mapM_ print n
      print "Enter your move"
      i <- readLn
      let mv = snd $ n!!(i-1)
      print mv
      print $ heuristic mv
      playAgainst d mv (choose tree mv) b (not p)

printInfo mv = do
  print $ move mv
  putStrLn $ "MaxDepth: " ++ show (maxDepth mv)
  putStrLn $ "MinDepth: " ++ show (minDepth mv)
  putStrLn $ "BestDepth: " ++ show (bestDepth mv)
  putStrLn $ "Heuristic: " ++ show (heuristic mv)
  putStrLn $ "Children: " ++ show (childs mv)
