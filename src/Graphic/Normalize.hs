module Graphic.Normalize where

import Codec.Picture
import Graphic.Image
import Control.Monad (forM_)

normalizeAll pathI pathO output = do
  img <- rImage pathI
  print "Read image"
  let t1 = grayscale img
  let t2' = fullScale t1
  let t2 = blackWhite t2' 145
  saveJpgImage 100 pathO (ImageRGB8 t2)
  print "Normailzed!"
  let ls = findAllWithPadding t2 (140,140) 20 (\a b -> test t2 a b > 30) (20,20)
  forM_ (zip ls [1..]) $ \((a,b),i) -> do
    putStrLn $ "Normalizing" ++ show (a,b,i)
    let t3 = normalize $ cut img a b
    saveJpgImage 100 (output ++ show i ++ ".jpg") (ImageRGB8 t3)
  print "Done Normalizing"
  return $ length ls

test img a b = countPixels img a b (\(PixelRGB8 r _ _) -> r<50)

rImage path = do
  a <- (convertRGB8 <$>) <$> readJpeg path
  case a of
    Right img -> return img
    Left a -> do
      print "Left"
      print a
      return undefined
