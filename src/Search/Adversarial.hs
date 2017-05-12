module Search.Adversarial where

data Node a = Node a Double

instance Eq (Node a) where
  (Node _ a) == (Node _ b) = a == b

instance Ord (Node a) where
  compare (Node _ a) (Node _ b) = compare a b

minMax :: (a -> Double) -> (a -> Bool) -> (a -> [a]) -> a -> Bool -> Int -> Maybe (Node a)
minMax heur goal suc node minM depth = if not (null s) then maxMin (step <$> suc node) else Nothing
  where
    maxMin = if minM then minimum else maximum
    s = step <$> suc node
    step n
      | depth == 0 = Just (Node n $ heur n)
      | goal n     = Just (Node n $ heur n)
      | otherwise  = minMax heur goal suc n (not minM) (depth-1)

--TODO: minMax with alpha beta cutoff
minMaxAB :: (a -> Double) -> (a -> Bool) -> (a -> [a]) -> a -> Bool -> Int -> Double -> Double -> Maybe (Node a)
minMaxAB = undefined
