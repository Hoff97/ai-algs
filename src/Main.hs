{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Graphics.UI.WX
import Data.IORef

main :: IO ()
main
  = start hello

hello :: IO ()
hello = do
  f    <- frame    [text := "Hello!"]
  quit <- button f [text := "Quit", on command := close f]
  img <- imageCreateFromFile (path ++ "1.jpg")
  bm <- bitmapFromImage img >>= newIORef
  c <- newIORef 1
  l :: IORef [(Int,Maybe Int)] <- newIORef []
  sw   <- scrolledWindow f [scrollRate := sz 100 100, on paint := onPaint bm
                            ,bgcolor := white, fullRepaintOnResize := False]
  set sw [on anyKey := t l c bm f]
  set f [layout := column 1 [hfill $ hrule 1 ,fill (widget sw)],
    clientSize := sz 300 200]
  where
    onPaint bm dc viewArea = do
      b <- readIORef bm
      drawBitmap dc b pointZero False []
    t l c img f k = do
      i <- readIORef c
      modifyIORef c (+1)
      if (head . show $ k) `elem` "0123456789" then do
        modifyIORef l ((i,Just . read . show $ k):)
        print "Added to index"
      else modifyIORef l ((i,Nothing):)
      if i < m then do
        bm <- bitmapCreateFromFile (path ++ show (i+1) ++ ".jpg")
        writeIORef img bm
        repaint f
      else do
        list <- readIORef l
        writeFile (path ++ "_result.txt") (show list)
        close f

path = "res/img/numbers/norm/testN"
m = 107
