module Examples.Minst where

import           Codec.Picture
import           Control.Monad
import           Data.Array
import           Graphic.LearnLoad
import           Learning.Backpropagation
import           Learning.NeuralNet
import           Minst.Data


main = do
  labels <- readLabels "res/minst/train-labels.idx1-ubyte"
  images <- readImages "res/minst/train-images.idx3-ubyte"
  let nnR = nn 0 [(28*28,28*28),(28*28,28*28),(10,28*28)]
  --nn :: NN Double <- randomNN [(28*28,28*28),(28*28,28*28),(10,28*28)]
  let zipped = zip [1..] $ zip labels images
  forMU zipped nnR $ \(n,(l,i)) nnR -> do
    putStrLn $ "Processing: " ++ show l ++ " (Image " ++ show n ++ ")"
    let input = toInput i (\c -> (fromIntegral c :: Double) / 255)
    let output = indexOutput (fromInteger l) 10
    let sim = value nnR f input
    let nn' = learn (-0.05) input output nnR f f'
    print sim
    print ("Difference",distance sim output)
    return nn'
  print "Yay"

--f x = x
--f' x = 1
f x = 1/(1+e**(-x))
f' x = (e**x)/((1+e**x)^2)

e = 2.718281828459045

t xs = (xs !!)

distance :: Num a => Vec a -> Vec a -> a
distance (Vec a) (Vec b) = sum $ map (\i -> abs $ a!i + b!i) [1..snd . bounds $ a]

--TODO: Create index for MINST Data Set, do Backpropagation Learning with it...

forMU :: Monad m => [a] -> b -> (a -> b -> m b) -> m b
forMU [] s f = return s
forMU (x:xs) s f = do
  s' <- f x s
  forMU xs s' f
