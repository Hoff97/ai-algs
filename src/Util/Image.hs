module Util.Image where

import Codec.Picture
import Codec.Picture.Types

interpolate :: Image PixelRGB8 -> Float -> Float -> PixelRGB8
interpolate img x y = fromFloats ((floatAt img (floor x) (floor y) +
  floatAt img (floor x) (ceiling y) + floatAt img (ceiling x) (floor y) +
  floatAt img (ceiling x) (ceiling y))/4)

data PixelFloat = PF Float Float Float

instance Num PixelFloat where
  (PF a b c) + (PF a' b' c') = PF (a+a') (b+b') (c+c')
  (PF a b c) * (PF a' b' c') = PF (a*a') (b*b') (c*c')
  abs (PF a b c) = PF (abs a) (abs b) (abs c)
  signum (PF a b c) = PF (signum a) (signum b) (signum c)
  negate (PF a b c) = PF (negate a) (negate b) (negate c)
  fromInteger x = PF (fromInteger x) (fromInteger x) (fromInteger x)

instance Fractional PixelFloat where
  fromRational x = PF (fromRational x) (fromRational x) (fromRational x)
  (PF a b c) / (PF a' b' c') = PF (a/a') (b/b') (c/c')

toFloats :: PixelRGB8 -> PixelFloat
toFloats (PixelRGB8 r g b) = PF (fromInteger $ toInteger r) (fromInteger $ toInteger g) (fromInteger $ toInteger b)

fromFloats (PF r g b) = PixelRGB8 (fromInteger . limit . floor $ r) (fromInteger . limit . floor $ g) (fromInteger . limit . floor $ b)
  where
    limit x
      | x>255 = 255
      | x<0 = 0
      |otherwise = x

floatAt img x y = toFloats $ pixelAt img x y

scale :: Image PixelRGB8 -> Int -> Int -> Image PixelRGB8
scale img x' y' = generateImage pos x' y'
  where
    pos i j = interpolate img (fromIntegral (i*x)/fromIntegral x') (fromIntegral (j*y)/fromIntegral y')
    x = imageWidth img
    y = imageHeight img

cut :: Pixel px => Image px -> (Int,Int) -> (Int,Int) -> Image px
cut img (a,b) (c,d) = generateImage pos (c-a) (d-b)
  where
    pos i j = pixelAt img (i+a) (j+b)

contrast :: Image PixelRGB8 -> Float -> Image PixelRGB8
contrast img c = generateImage pos x y
  where
    pos i j = fromFloats (floatAt img i j * PF c c c)
    x = imageWidth img
    y = imageHeight img

brightness :: Image PixelRGB8 -> Float -> Image PixelRGB8
brightness img c = generateImage pos x y
  where
    c' = (2*c-1)*255
    pos i j = fromFloats (floatAt img i j + PF c' c' c')
    x = imageWidth img
    y = imageHeight img
