module Minst.Data where

import Data.ByteString as B
import Codec.Picture
import Control.Monad
import Debug.Trace

readLabels :: FilePath -> IO [Integer]
readLabels path = do
  c <- B.readFile path
  let rs = B.drop 8 c
  return . map' [1..B.length rs] $ \i -> toInteger (B.index rs (i-1))

map' = flip Prelude.map

readImages :: FilePath -> IO [Image Pixel8]
readImages path = do
  c <- B.readFile path
  let rs = B.drop 16 c
  return . map' [0..60000-1] $ \i ->
    let pixels = B.take (28*28) . B.drop (i*28*28) $ rs
    in generateImage (\x y -> B.index pixels (x+y*28)) 28 28
