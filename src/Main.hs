{-# LANGUAGE ScopedTypeVariables #-}

module Main where

--import Graphics.UI.WX
import Data.IORef
import Graphic.Normalize
import Graphic.Categorize
import Graphic.LearnLoad
import Learning.NeuralNet
import Learning.Backpropagation
import Codec.Picture

main = do
  ls <- loadIndex 10 "res/img/numbers/index/N" (\(PixelRGB8 r _ _) -> fromIntegral (r-255) / 255)
  let ls' = (\(a,b,c) -> (b,c)) <$> ls
  nn <- ((/100) <$>) <$> randomNN [(400,400),(400,400),(10,400)]
  print $ value nn id (fst (ls'!!0))
  let nn' = learnMultiple (-0.001) nn id (const 1) ls'
  print $ value nn' id (fst (ls'!!0))
  print "yay"


--TODO: Create index for MINST Data Set, do Backpropagation Learning with it...
{-main :: IO ()
main = do
  n <- normalizeAll "res/img/numbers/test1.jpg" "res/img/numbers/normalized.jpg" "res/img/numbers/norm/N"
  start $ categorize "res/img/numbers/norm/N" n s index

index :: [(Int,Maybe Int)] -> IO ()
index ls = do
  addToIndex "res/img/numbers/norm/N" ls "res/img/numbers/index/N"
  return ()-}
