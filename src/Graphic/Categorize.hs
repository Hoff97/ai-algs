{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts#-}

module Graphic.Categorize where

{-import Graphics.UI.WX
import Data.IORef
import Control.Monad (forM_)
import System.Directory (copyFile)

categorize :: FilePath -> Int -> (Key -> Maybe a) -> ([(Int,Maybe a)] -> IO ()) -> IO ()
categorize path m keyHandle done = do
  print "Starting Categorizing"
  f    <- frame    [text := "Hello!"]
  img <- imageCreateFromFile (path ++ "1.jpg")
  bm <- bitmapFromImage img >>= newIORef
  c <- newIORef 1
  l :: IORef [(Int,Maybe a)] <- newIORef []
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
      modifyIORef l ((i,keyHandle k):)
      print $ "Added Key Handle for " ++ show k
      if i < m then do
        bm <- bitmapCreateFromFile (path ++ show (i+1) ++ ".jpg")
        writeIORef img bm
        repaint f
      else do
        list <- readIORef l
        close f
        print "Done Categorizing"
        done list

addToIndex :: (Show a, Read a) => FilePath -> [(Int,Maybe a)] -> FilePath -> IO [a]
addToIndex input ls output = do
  os <- (reverse . read) <$> readFile (output ++ "_label.txt")
  print os
  osR <- newIORef os
  forM_ ls $ \(i,m) -> case m of
    Just a -> do
      l <- length <$> readIORef osR
      modifyIORef osR (a:)
      copyFile (input ++ show i ++ ".jpg") (output ++ show (l+1) ++ ".jpg")
    Nothing -> return ()
  os' <- reverse <$> readIORef osR
  writeFile (output ++ "_label.txt") (show os')
  return os'


s x
  | (head . show $ x) `elem` "0123456789" = Just . read . show $ x
  | otherwise = Nothing-}
