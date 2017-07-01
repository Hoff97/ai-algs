module Util.Image where

import Codec.Picture
import Codec.Picture.Types
import Control.Lens.Traversal
import Control.Lens
import Debug.Trace

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
    pos i j = pixelAt img (min (x-1) $ i+a) (min (y-1) $ j+b)
    x = imageWidth img
    y = imageHeight img

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

grayscale :: Image PixelRGB8 -> Image PixelRGB8
grayscale img = generateImage pos x y
  where
    y' (PF r g b) = 0.299*r+0.587*g+0.114*b
    pos i j = let g' = y' $ floatAt img i j in fromFloats (PF g' g' g')
    x = imageWidth img
    y = imageHeight img

minMaxBrightness :: Image PixelRGB8 -> (Int,Int)
minMaxBrightness img = foldr boundsUpdate (300,-100 ) t
  where
    boundsUpdate x (mini,maxi)
      | x < mini && x > maxi = (x,x)
      | x < mini = (x,maxi)
      | x > maxi = (mini,x)
      | otherwise = (mini,maxi)
    t = (\(PixelRGB8 r _ _) -> fromIntegral r) <$> view (partsOf imagePixels) img

fullScale :: Image PixelRGB8 -> Image PixelRGB8
fullScale img = generateImage pos x y
  where
    pos i j = fromFloats ((floatAt img i j - fromIntegral a)*PF scale scale scale)
    scale = 255/fromIntegral (b - a)
    (a,b) = minMaxBrightness img
    x = imageWidth img
    y = imageHeight img

blackWhite :: Image PixelRGB8 -> Int -> Image PixelRGB8
blackWhite img b = generateImage pos x y
  where
    pos i j = let (PixelRGB8 r _ _) = pixelAt img i j
      in if r <= fromIntegral b then PixelRGB8 0 0 0 else PixelRGB8 255 255 255
    x = imageWidth img
    y = imageHeight img

boundsFor :: Image PixelRGB8 -> (PixelRGB8 -> Bool) -> ((Int,Int),(Int,Int))
boundsFor img f = foldr updateBounds ((x+1,y+1),(-1,-1)) t
  where
    updateBounds (i,j,p) ((a,b),(c,d))
      | not (f p) = ((a,b),(c,d))
      | otherwise = ((min a i,min b j),(max c i,max d j))
    t = [(i,j,pixelAt img i j) | i <- [0..x-1],  j <- [0..y-1]]
    x = imageWidth img
    y = imageHeight img

boundsSum :: Image PixelRGB8 -> (Int,Int) -> (Int,Int) -> Int
boundsSum img (a,b) (c,d) = foldr sumB 0 t
  where
    sumB (i,j,PixelRGB8 r _ _) a = a + fromIntegral r
    t = [(i,j,pixelAt img i j) | i <- [a..c],  j <- [b..d]]
    x = imageWidth img
    y = imageHeight img

countPixels :: Image PixelRGB8 -> (Int,Int) -> (Int,Int) -> (PixelRGB8 -> Bool) -> Int
countPixels img (a,b) (c,d) f = foldr sumC 0 t
  where
    sumC p a = if f p then a+1 else a
    t = [pixelAt img i j | i <- [max 1 a..min (x-1) c],  j <- [max 1 b..min (y-1) d]]
    x = imageWidth img
    y = imageHeight img

findWithPadding :: Image PixelRGB8 -> (Int,Int) -> Int -> ((Int,Int) -> (Int,Int) -> Bool) -> (Int,Int) -> ((Int,Int),(Int,Int))
findWithPadding img (s,t) p f = go
  where
    go (x,y)
      | a && not (rt || r || rl || l || u || left) = ((x,y),(x+s,y+t))
      | r = go $ step (x+p,y)
      | otherwise = go $ step (x+s+p,y)
      where
        rt = f (x+s,y-p) (x+s+p,y)
        r = f (x+s,y) (x+s+p,y+t)
        rl = f (x+s,y+t) (x+s+p,y+t+p)
        l = f (x,y+t) (x+s,y+t+p)
        a = f (x,y) (x+s,y+t)
        u = f (x-p,y-p) (x+s,y)
        left = f (x-p,y) (x,y+t+p)
        step (x,y) = if x+s>w then (0,y+p) else (x,y)
    w = imageWidth img
    h = imageHeight img

findAllWithPadding :: Image PixelRGB8 -> (Int,Int) -> Int -> ((Int,Int) -> (Int,Int) -> Bool) -> (Int,Int) -> [((Int,Int),(Int,Int))]
findAllWithPadding img (s,t) p f start
  | c+p+s > w && d+p+t > h = [((a,b),(c,d))]
  | c+p+s > w = ((a,b),(c,d)) : findAllWithPadding img (s,t) p f (p,d+p)
  | otherwise = ((a,b),(c,d)) : findAllWithPadding img (s,t) p f (c+p,b)
  where
    ((a,b),(c,d)) = findWithPadding img (s,t) p f start
    w = imageWidth img
    h = imageHeight img


normalize :: Image PixelRGB8 -> Image PixelRGB8
normalize img = scaled
  where
    grey = grayscale img
    full = fullScale grey
    bW = blackWhite full 180
    ((a,b),(c,d)) = boundsFor bW dark
    dark (PixelRGB8 r _ _) = r < 100
    cutted = cut bW (max 0 (a-pad),max 0 (b-pad)) (min (x-1) (c+pad),min (y-1) (d+pad))
    scaled = scale cutted 20 20
    pad = 5
    x = imageWidth img
    y = imageHeight img
