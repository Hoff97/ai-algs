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
wP' = White (Pawn True)
bP = Black (Pawn False)
bP' = Black (Pawn True)
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
ee = Empty

enPassant (Black (Pawn b)) = b
enPassant (White (Pawn b)) = b
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

clear :: CBoard -> (Int,Int) -> Bool
clear (Board a p) (i,j)
  | i<0 || j<0 || i>7 || j>7 = True
  | otherwise = empty $ a!(i,j)

start :: CBoard
start = Board (listArray ((0,0),(7,7)) $ [
                  wR,wN,wB,wK,wQ,wB,wN,wR,
                  wP,wP,wP,wP,wP,wP,wP,wP
                                       ] ++ replicate 32 Empty ++
              [
                bP,bP,bP,bP,bP,bP,bP,bP,
                bR,bN,bB,bK,bQ,bB,bN,bR
              ]) True

test :: CBoard
test = Board (listArray ((0,0),(7,7)) $ [
                  wR,wN,wB,wK,wQ,wB,wN,wR,
                  ee,wP,ee,ee,wP,wP,wP,wP,
                  ee,ee,ee,ee,ee,ee,ee,ee,
                  wP,bP,ee,ee,ee,ee,ee,ee] ++ replicate 16 Empty ++
              [
                bP,bP,bP,ee,bP,bP,bP,bP,
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

makeP :: CField -> CField
makeP (White (Pawn _)) = White (Pawn True)
makeP (Black (Pawn _)) = Black (Pawn True)
makeP a = a

unmakeP :: CField -> CField
unmakeP (White (Pawn _)) = White (Pawn False)
unmakeP (Black (Pawn _)) = Black (Pawn False)
unmakeP a = a

pawnM (Board a p) (i,j) = pawnMove ++ pawnBeat
  where
    piece = a!(i,j)
    pawnMove = (dir,0):(if i == baseLine then [(dir*2,0)] else []) >>= (moveP . ((i,j)+))
    pawnBeat = [(dir,-1),(dir,1)] >>= (beatP . ((i,j)+))
    dir = if white piece then 1 else -1
    baseLine = if white piece then 1 else 7
    beatP (c,d)
        | c<0 || d < 0 || c>7 || d>7 = []
        | not (empty p') && color piece /= color p' && not (king p') = [(Board (a//[((i,j),Empty),((c,d),unmakeP piece)]) (not p),c,d)]
        | empty p' && not (empty pessant) && color pessant /= color piece && enPassant pessant = [(Board (a//[((i,j),Empty),((c,d),unmakeP piece),((c-dir,d),Empty)]) (not p),c,d)]
        | otherwise = []
          where
            p' = a!(c,d)
            pessant = a!(c-dir,d)
    moveP (c,d)
      | c<0 || d < 0 || c>7 || d>7 = []
      | empty p' = [(Board (a//[((i,j),Empty),((c,d),if (i-c) == 2 then makeP piece else unmakeP piece)]) (not p),c,d)]
      | otherwise = []
      where p' = a!(c,d)

walkKing g@(Board a p) (i,j) f
  | c<0 || d<0 || c>7 || d>7 = []
  | empty (a!(c,d)) && allClear = [(Board (a//[((i,j),Empty),((c,d),unRochade piece)]) (not p),c,d)]
  | otherwise = []
  where
    piece = a!(i,j)
    (c,d) = f (i,j)
    allClear = dirClear && knightClear
    knightClear = all (notEnemyKnight . ((c,d)+)) [(-2,-1),(-1,-2),(2,1),(1,2),(-1,2),(2,-1),(-2,1),(1,-2)]
    dirClear = all (walkSafe (c,d)) [up,down,left,right,rup,lup,rdown,ldown]
    notEnemyKnight (i',j')
      | i'<0 || j'<0 || i'>7 || j' > 7 = True
      | otherwise = not (knight (a!(i',j')) && color (a!(i',j')) /= color piece)
    walkSafe (i',j') f
      | i'<0 || j'<0 || i'>7 || j' > 7 = True
      | empty $ a!(i',j') = walkSafe (f (i',j')) f
      | color piece == color (a!(i',j')) = True
      | (queen (a!(i',j')) || bishop (a!(i',j'))) && abs (c-i') == abs (d-j') = False
      | (queen (a!(i',j')) || rook (a!(i',j'))) && ((c-i')*(d-j')==0) = False
      | (king (a!(i',j')) && distance (i',j') (c,d) == 1) = False
      | pawn (a!(i',j')) && abs (d-j') == 1 && c-i' == (if white (a!(i',j')) then 1 else -1) = False
      | otherwise = True

distance (a,b) (c,d) = max (a-c) (b-d)

unRochade :: CField -> CField
unRochade (White (King _)) = White (King False)
unRochade (Black (King _)) = Black (King False)
unRochade a = a

rochadeKing g (i,j) = undefined

moves :: CBoard -> (Int,Int) -> [(CBoard,Int,Int)]
moves g@(Board a p) (i,j)
  | pawn piece = pawnM g (i,j)
  | bishop piece = [rup,lup,rdown,ldown] >>= walk (i,j)
  | knight piece = [(-2,-1),(-1,-2),(2,1),(1,2),(-1,2),(2,-1),(-2,1),(1,-2)] >>= (moveOrBeat . ((i,j)+))
  | rook piece = [up,down,left,right] >>= walk (i,j)
  | queen piece = [up,down,left,right,rup,lup,rdown,ldown] >>= walk (i,j)
  | king piece = ([up,down,left,right,rup,lup,rdown,ldown] >>= (walkKing g (i,j))) ++ rochadeKing g (i,j)
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
