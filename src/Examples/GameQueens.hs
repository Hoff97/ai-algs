module Examples.GameQueens where

import           Data.Array
import           Data.Foldable
import           Search.Adversarial

data QPiece = Stone | Queen

data Field a = Black a | White a | Empty

black :: Field t -> Bool
black (Black _) = True
black _         = False

white :: Field t -> Bool
white (White _) = True
white _         = False

empty :: Field t -> Bool
empty Empty = True
empty _     = False

data Board a = Board (Array (Int,Int) a) Bool

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

end :: QBoard -> Bool
end (Board a _) = onlyBlack || onlyWhite
  where
    onlyBlack = foldr (\a b -> b && (black a || empty a)) True a
    onlyWhite = foldr (\a b -> b && (white a || empty a)) True a
