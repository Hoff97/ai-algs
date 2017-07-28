{-# LANGUAGE ScopedTypeVariables #-}

module Graphic.LearnLoad where

import Codec.Picture
import Learning.NeuralNet
import Data.Array (listArray)
import Debug.Trace

toInput :: Image PixelRGB8 -> (PixelRGB8 -> Double) -> Vec Double
toInput img f = Vec $ listArray (1,x*y) pixels
  where
    pixels = [f $ pixelAt img i j | i <- [0..(x-1)], j <- [0..(y-1)]]
    x = imageWidth img
    y = imageHeight img

indexOutput :: Int -> Int -> Vec Double
indexOutput i x = Vec $ listArray (1,x) (map (\j -> if j==i then 1 else 0) [1..x])

loadIndex :: (Integral a, Read a) => Int -> FilePath -> (PixelRGB8 -> Double) -> IO [(a,Vec Double, Vec Double)]
loadIndex s path f = do
  ls :: [a] <- read <$> readFile (path ++ "_label.txt")
  fs <- sequenceA $ (\i -> readJpeg (path ++ show i ++ ".jpg")) <$> [1..length ls]
  let r = map (extract s f) $ zip fs ls
  return r

extract :: Integral a => Int -> (PixelRGB8 -> Double) -> (Either String DynamicImage, a) -> (a,Vec Double,Vec Double)
extract s f (Right (ImageRGB8 img),a) = (a,toInput img f, indexOutput (fromInteger . toInteger $ a) s)
extract s f (Left a,b) = trace a undefined
extract s f (Right x,a) = let img = convertRGB8 x in (a,toInput img f, indexOutput (fromInteger . toInteger $ a) s)
