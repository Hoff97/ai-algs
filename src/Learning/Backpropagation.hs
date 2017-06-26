module Learning.Backpropagation where

import Data.Array
import Math.Matrix
import Learning.NeuralNet
import Debug.Trace

(&!) :: Vec a -> Int -> a
(Vec a) &! i = a!i

deltas :: Num a => NN a -> Vec a -> (a -> a) -> Array Int (Vec (a,a)) -> Array Int (Vec a)
deltas (NN nn) dOutput deriv inOuts = result
  where
    result = array (a,b) [(i,if i == b then output else layer i) | i <- [a..b]]
    output = (deriv . fst <$> (inOuts!(b+1)))*(dOutput - (snd <$> (inOuts!(b+1))))
    layer i = let (c,d) = dim (nn!(i+1)) in
      (deriv . fst <$> (inOuts!(i+1)))*Vec (array (1,d) [(j,sumOf i j c) | j <- [1..d]])
    sumOf i j c = sum $ (\k -> ((result!(i+1)) &! k)*((nn!(i+1))&(k,j))) <$> [1..c]
    (a,b) = bounds nn

deltaW :: Num a => a -> NN a -> Array Int (Vec (a,a)) -> Array Int (Vec a) -> NN a
deltaW lR (NN nn) inOuts deltas = NN $ array (a,b) [(i,layer i) | i <- [a..b]]
  where
    layer i = let (c,d) = dim (nn!i) in matrix (c,d) [(k,l,dW i k l) | k <- [1..c], l <- [1..d]]
    dW i k l = -lR * ((deltas!i)&!k) * snd ((inOuts!i)&!l)
    (a,b) = bounds nn

--TODO:This only seems to work with negative learning rates... Why?
learn :: Num a => a -> Vec a -> Vec a -> NN a -> (a -> a) -> (a -> a) -> NN a
learn lR input output nn@(NN nn') f f' = NN $ array (a,b) [(i,layer i) | i <- [a..b]]
  where
    layer i = let (c,d) = dim (nn'!i) in matrix (c,d) [(k,l,at i k l) | k <- [1..c], l <- [1..d]]
    at i k l = ((nn'!i)&(k,l)) + ((dW!i)&(k,l))
    vS = values nn f input
    dS = deltas nn output f' vS
    NN dW = deltaW lR nn vS dS
    (a,b) = bounds nn'

learnMultiple :: (Show a, Num a) => a -> NN a -> (a -> a) -> (a -> a) -> [(Vec a, Vec a)] -> NN a
learnMultiple lR nn f f' [] = nn
learnMultiple lR nn f f' ((input,output):xs) = learnMultiple lR nn' f f' xs
  where
    nn' = traceShow ("Iteration",length xs,nn) $ learn lR input output nn f f'
