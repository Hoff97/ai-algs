module Examples.GridSearch where

import Graphics.Gloss
import Data.Array

newtype Grid = Grid (Array (Int,Int) Bool)

drawGrid :: Grid -> Color -> Picture
drawGrid (Grid a) c =
  Pictures ((\((i,j),b) -> if b
    then color c . translate (fromIntegral i*10) (fromIntegral j*10) $ rectangleSolid 10 10
    else blank) <$> assocs a)

data State a = State { grid :: Grid, position :: (Int,Int), visited :: Grid, info :: a }

test = Grid (listArray ((1,1),(3,3)) [True,False,False,False,False,True,True,True,True])
