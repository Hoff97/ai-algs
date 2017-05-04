module Examples.GridSearch where

import Graphics.Gloss
import Data.Array
import Search.Online
import Debug.Trace
import System.Random

newtype Grid = Grid (Array (Int,Int) Bool)

drawGrid :: Grid -> Color -> Picture
drawGrid (Grid a) c =
  Pictures ((\((i,j),b) -> if b
    then color c . translate (fromIntegral i*10) (fromIntegral j*10) $ rectangleSolid 10 10
    else blank) <$> assocs a)

data State a = State { grid :: Grid, position :: (Int,Int), visited :: Grid, info :: a }

drawState :: State a -> Picture
drawState (State g (x,y) v _) = Pictures [
  drawGrid v green,drawGrid g black,
  color red . translate (fromIntegral x*10) (fromIntegral y*10) $ rectangleSolid 10 10]

traceR x = traceShow x x

doLrta :: State (Array (Int,Int) Double) -> State (Array (Int,Int) Double)
doLrta s@(State (Grid g) p (Grid v) i) = case lrta p succsessor (i!) (const . const $ 2) of
  Just (d,a) -> State (Grid g) a (Grid $ v//[(p,True)]) (i//[(p,traceR d)])
  Nothing -> s
  where
    succsessor (x,y) = filter (\p -> inBounds (bounds v) p && not (g!p)) (list x y)
    list x y = if x > y then [(x,y-1),(x,y+1),(x-1,y),(x+1,y)] else [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

inBounds :: ((Int,Int),(Int,Int)) -> (Int,Int) -> Bool
inBounds ((x1,y1),(x2,y2)) (x,y) = x>=x1 && y>=y1 && x<=x2 && y<=y2

t = True
f = False

vis = Grid $ listArray ((1,1),(sizeX,sizeY)) [False | i <- [1..sizeX], j <- [1..sizeY]]

initState g = State g (2,2) vis $
  listArray ((1,1),(sizeX,sizeY)) [fromIntegral $ (sizeX-1-j) + (sizeY-1-i) | i <- [1..sizeX], j <- [1..sizeY]]

test = do
  g <- grid2
  simulate (InWindow "Yay" (400,400) (200,200)) white 20 (initState $ Grid g) drawState (\_ _ m -> doLrta m)

sizeX = 300
sizeY = 300

grid2 = listArray ((1,1),(sizeX,sizeY)) <$> sequenceA [if x==1||y==1||x==sizeX||y==sizeY then return True
  else (< (0.3 :: Double)) <$> randomIO | x <- [1..sizeX], y <- [1..sizeY]]
