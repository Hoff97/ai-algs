{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Examples.GameQueens where

import           Data.Array
import           Data.Foldable
import           Debug.Trace
import           Search.Adversarial
import           Util.EnumHelp

data QPiece = Stone | Queen deriving Eq

data Field a = Black a | White a | Empty deriving Eq

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

data Board a = Board (Array (Int,Int) a) Bool

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
start = Board (listArray ((0,0),(7,3)) $ replicate 12 (White Stone) ++ [Empty,Black Stone] ++ replicate 6 Empty ++ replicate 5 (Black Stone) ++ [Empty] ++ replicate 6 (Black Stone)) True

end :: QBoard -> Bool
end (Board a _) = onlyBlack || onlyWhite
  where
    onlyBlack = foldr (\a b -> b && (black a || empty a)) True a
    onlyWhite = foldr (\a b -> b && (white a || empty a)) True a

--TODO: Handle Queens
next :: QBoard -> [QBoard]
next (Board a p) = if not (null beat) then map (\a -> Board a (not p)) beat else map (\a -> Board a (not p)) simpleMove
  where
    beat = concat $ map (checkBeatMove a) [(0,0)..(7,3)]
    checkBeatMove a (i,j)
      | j < 0 || j > 3 || i < 0 || i > 7 = []
      | a!(i,j) == Empty = []
      | black (a!(i,j)) && p = []
      | white (a!(i,j)) && not p = []
      | queen (a!(i,j)) = []
      | otherwise = (if p then [lu,ru] else [ld,rd]) >>= beatTo (i,j) a
    beatTo (i,j) a f
      | j'<0 || j'>3 || i'<0 || i'>7 = []
      | a!(i',j') == Empty && a!(c,d) /= Empty && color (a!(c,d)) /= color (a!(i,j)) = [a // [((i,j),Empty),((c,d),Empty),((i',j'),a!(i,j))]] >>= maybeBeat (i',j')
      | otherwise = []
        where
          maybeBeat (i,j) a = case checkBeatMove a (i,j) of
            [] -> [a]
            ls -> ls
          (c,d) = f (i,j)
          (i',j') = f (c,d)

    simpleMove = concat $ map checkMove [(0,0)..(7,3)]
    checkMove (i,j)
      | j < 0 || j > 3 || i < 0 || i > 7 = []
      | a!(i,j) == Empty = []
      | black (a!(i,j)) && p = []
      | white (a!(i,j)) && not p = []
      | queen (a!(i,j)) = []
      | otherwise = (if p then [lu (i,j), ru (i,j)] else [ld (i,j), rd (i,j)]) >>= moveTo (i,j)
    moveTo (i,j) (i',j')
      | j'<0 || j'>3 || i'<0 || i'>7 = []
      | a!(i',j') == Empty = [a // [((i,j),Empty),((i',j'),a!(i,j))]]
      | otherwise = []

lu :: (Int,Int) -> (Int,Int)
lu (i,j) = (i+1,j+((i+1) `mod` 2)-1)

ld :: (Int,Int) -> (Int,Int)
ld (i,j) = (i-1,j+((i+1) `mod` 2)-1)

ru :: (Int,Int) -> (Int,Int)
ru (i,j) = (i+1,j+((i+1) `mod` 2))

rd :: (Int,Int) -> (Int,Int)
rd (i,j) = (i-1,j+((i+1) `mod` 2))
