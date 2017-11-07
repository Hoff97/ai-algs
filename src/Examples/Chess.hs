{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Examples.Chess where

import           Data.Array
import           Data.Char          (toLower)
import           Data.Foldable
import           Data.Hashable
import           Data.Maybe         (fromMaybe)
import           Debug.Trace
import           GHC.Generics
import           Search.Adversarial
import           Search.Adversarial
import           Util.EnumHelp
import           Util.Memoize
import           Util.NumClass

data CPiece = Pawn Bool | Rook Bool | Queen | King Bool | Bishop | Knight  deriving (Eq, Generic)

instance Show CPiece where
  show (Pawn _) = "P"
  show (Rook _) = "R"
  show Queen    = "Q"
  show (King _) = "K"
  show Bishop   = "B"
  show Knight   = "N"

instance Hashable CPiece

data Field a = Black a | White a | Empty deriving (Eq, Generic)

instance Hashable a => Hashable (Field a)

instance Show (Field CPiece) where
  show (White p) = show p
  show (Black p) = map toLower $ show p
  show Empty     = " "

piece :: Field a -> Maybe a
piece (Black a) = Just a
piece (White a) = Just a
piece _         = Nothing

black :: Field t -> Bool
black (Black _) = True
black _         = False

white :: Field t -> Bool
white (White _) = True
white _         = False

empty :: Field t -> Bool
empty Empty = True
empty _     = False

queen :: Field CPiece -> Bool
queen = fromMaybe False . ((== Queen) <$>) . piece

king :: Field CPiece -> Bool
king (Black (King _)) = True
king (White (King _)) = True
king _                = False

pawn :: Field CPiece -> Bool
pawn (Black (Pawn _)) = True
pawn (White (Pawn _)) = True
pawn _                = False

rook :: Field CPiece -> Bool
rook (Black (Rook _)) = True
rook (White (Rook _)) = True
rook _                = False

bishop :: Field CPiece -> Bool
bishop = fromMaybe False . ((== Bishop) <$>) . piece

knight :: Field CPiece -> Bool
knight = fromMaybe False . ((== Knight) <$>) . piece

color :: Field CPiece -> Field ()
color (Black _) = Black ()
color (White _) = White ()
color Empty     = Empty

wP = White (Pawn False)
bP = Black (Pawn False)
wB = White Bishop
bB = Black Bishop
wN = White Knight
bN = Black Knight
wR = White (Rook True)
bR = Black (Rook True)
wQ = White Queen
bQ = Black Queen
wK = White (King True)
bK = Black (King True)

enPassant (Pawn b) = b
enPassant _        = False

rochade (King b) = b
rochade (Rook b) = b
rochade _        = False

data Board a = Board (Array (Int,Int) a) Bool deriving (Eq, Generic)

board :: Board a -> Array (Int, Int) a
board (Board a _) = a

instance Hashable a => Hashable (Board a) where
  hashWithSalt salt (Board a b) = hashWithSalt salt (elems a,b)

instance Heuristic CBoard where
  heuristic (Board a _) = foldr ((+) . heuristic) 0 a

instance Show CBoard where
  show (Board a _) = " +A+B+C+D+E+F+G+H+\n" ++ concat rows ++ " +A+B+C+D+E+F+G+H+"
    where
      rows = map (\(i,j) -> "|" ++ show (a!(i,j)) ++ (if j==0 then "|" ++ show (i+1) ++ "\n" else "")) (reverse [(0,0)..(7,7)])

instance Heuristic CPiece where
  heuristic (Pawn _) = 1
  heuristic Bishop   = 3
  heuristic Knight   = 3
  heuristic (Rook _) = 5
  heuristic Queen    = 9
  --The king doesnt get counted normally
  heuristic (King _) = 0

instance Heuristic a => Heuristic (Field a) where
  heuristic (Black p) = inverted p
  heuristic (White p) = heuristic p
  heuristic Empty     = 0

type CField = Field CPiece
type CBoard = Board CField

start :: CBoard
start = Board (listArray ((0,0),(7,7)) $ [
                  wR,wN,wB,wK,wQ,wB,wN,wR,
                  wP,wP,wP,wP,wP,wP,wP,wP
                                       ] ++ replicate 32 Empty ++
              [
                bP,bP,bP,bP,bP,bP,bP,bP,
                bR,bN,bB,bK,bQ,bB,bN,bR
              ]) True

up :: (Int,Int) -> (Int,Int)
up (i,j) = (i+1,j)

down :: (Int,Int) -> (Int,Int)
down (i,j) = (i-1,j)

right :: (Int,Int) -> (Int,Int)
right (i,j) = (i,j+1)

left :: (Int,Int) -> (Int,Int)
left (i,j) = (i,j-1)

lup :: (Int,Int) -> (Int,Int)
lup (i,j) = (i+1,j-1)

rup :: (Int,Int) -> (Int,Int)
rup (i,j) = (i+1,j+1)

ldown :: (Int,Int) -> (Int,Int)
ldown (i,j) = (i-1,j-1)

rdown :: (Int,Int) -> (Int,Int)
rdown (i,j) = (i-1,j+1)

moves :: CBoard -> (Int,Int) -> [(CBoard,Int,Int)]
moves (Board a p) (i,j)
  | pawn piece = undefined --Handle moving 2 at start + enpassant
  | bishop piece = [rup,lup,rdown,ldown] >>= walk (i,j)
  | knight piece = [(-2,-1),(-1,-2),(2,1),(1,2),(-1,2),(2,-1),(-2,1),(1,-2)] >>= (moveOrBeat . ((i,j)+))
  | rook piece = [up,down,left,right] >>= walk (i,j)
  | queen piece = [up,down,left,right,rup,lup,rdown,ldown] >>= walk (i,j)
  | king piece = undefined --Note: King has to handle rochade + being able to move to a place
  | otherwise = trace "Error - Chess.moves: A non-standard piece was found!!" []
    where
      walk (c,d) f
        | c<0 || d < 0 || c>7 || d>7 = []
        | c == i && d == j = walk (f (c,d)) f
        | empty p' = moveOrBeat (c,d) ++ walk (f (c,d)) f
        | color piece /= color p' && not (king p') = moveOrBeat (c,d)
        | otherwise = []
          where
            p' = a!(c,d)
      moveOrBeat (c,d)
        | c<0 || d < 0 || c>7 || d>7 = []
        | empty p' = [(Board (a//[((i,j),Empty),((c,d),piece)]) (not p),c,d)]
        | color piece /= color p' && not (king p') = [(Board (a//[((i,j),Empty),((c,d),piece)]) (not p),c,d)]
        | otherwise = []
          where
            p' = a!(c,d)
      piece = a!(i,j)
