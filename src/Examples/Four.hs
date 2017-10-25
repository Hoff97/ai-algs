{-# LANGUAGE DeriveGeneric #-}

module Examples.Four where

import           Data.Array
import           Data.Hashable
import           Debug.Trace
import           GHC.Generics
import           Search.Adversarial
import           Util.Memoize
import           Util.Tuples

data Field = X | O | Empty deriving (Eq, Generic)

instance Show Field where
  show X     = "X"
  show O     = "O"
  show Empty = " "

instance Hashable Field

data Four = Four Bool (Array (Int,Int) Field) deriving (Eq, Generic)

instance Show Four where
  show (Four _ b) = "#1234567#" ++ content ++ "\n#1234567#"
    where
      content = foldr addLine "" [[b ! (i,j) | j <- [1..7]] | i <- [1..6]]
      addLine a b = b ++ "\n|" ++ cLine a ++ "|"
      cLine l = foldr ((++) . show) "" l

instance Heuristic Field where
  heuristic Empty = 0
  heuristic X     = 1
  heuristic O     = -1

instance Hashable Four where
  hashWithSalt salt (Four a b) = hashWithSalt salt (a,elems b)

instance Heuristic Four where
  heuristic = heur

start :: Four
start = Four True (listArray ((1,1),(6,7)) $ replicate (6*7) Empty)

xwins = (foldr (.) id) (map (place') $ reverse [3,3,4,4]) start

place :: Four -> Int -> Four
place i@(Four b a) j
  | firstEmpty <= 6 = Four (not b) (a // [((firstEmpty,j),if b then X else O)])
  | otherwise = i
    where
      firstEmpty = foldr findEmpty 7 [6,5,4,3,2,1]
      findEmpty c i
        | i <= 6 = i
        | a ! (c,j) == Empty = c
        | otherwise = i

place' = flip place

next :: Four -> [Four]
next f = filter (/= f) $ map (place f) [1..7]

heur :: Four -> Double
heur (Four b a)
  | xw == 4 = 10000
  | yw == 4 = -10000
  | otherwise = x3*50 - y3*50 + weighted
  where
    weighted = 7*heuristic [a!(i,4) | i <- [1..6]] + 3*(heuristic $ [a!(i,3) | i <- [1..6]] ++ [a!(i,5) | i <- [1..6]])
    full = all (/= Empty) (elems a)
    walkPath = concat [[a ! (i,j) | j <- [1..7]] ++ [Empty] | i <- [1..6]]
      ++ concat [[a ! (i,j) | i <- [1..6]] ++ [Empty] | j <- [1..7]]
      ++ pathDiag 3 1 [] ++ pathDiag' 3 1 []
    pathDiag i j ls
      | j == 8 && i == 5 = ls
      | i<1 || j<1 = pathDiag (i+1) (j+1) ls
      | j > 7 = pathDiag (i - i+1 + (if i+1==j then 0 else -1)) (j - i+1  + (if i+1==j then 1 else 0)) (Empty : ls)
      | i > 6 = pathDiag (i - i+1) (j - i+2) (Empty : ls)
      | otherwise = pathDiag (i+1) (j+1) (a ! (i,j) : ls)
    pathDiag' i j ls
      | j == 8 && i == 5 = ls
      | i<1 || j<1 = pathDiag' (i+1) (j+1) ls
      | j > 7 = pathDiag' (i - i+1 + (if i+1==j then 0 else -1)) (j - i+1  + (if i+1==j then 1 else 0)) (Empty : ls)
      | i > 6 = pathDiag' (i - i+1) (j - i+2) (Empty : ls)
      | otherwise = pathDiag' (i+1) (j+1) (a ! (i,8-j) : ls)
    (xw,yw,x3,y3) = foldr walkIt (0,0,0,0) walkPath
    walkIt _ (4,j,x,y)     = (4,j,x,y)
    walkIt _ (i,4,x,y)     = (i,4,x,y)
    walkIt Empty (i,j,x,y) = if i == 3 then (0,0,x+1,y) else if j == 3 then (0,0,x,y+1) else (0,0,x,y)
    walkIt X (i,j,x,y)     = (i+1,0,x,if j == 3 then y+1 else y)
    walkIt O (i,j,x,y)     = (0,j+1,if i== 3 then x+1 else x,y)

end :: Four -> Bool
end (Four b a) = full || four
  where
    full = all (/= Empty) (elems a)
    four = fst walkRes == 4 || snd walkRes == 4
    walkPath = concat [[a ! (i,j) | j <- [1..7]] ++ [Empty] | i <- [1..6]]
      ++ concat [[a ! (i,j) | i <- [1..6]] ++ [Empty] | j <- [1..7]]
      ++ pathDiag 3 1 [] ++ pathDiag' 3 1 []
    pathDiag i j ls
      | j == 8 && i == 5 = ls
      | i<1 || j<1 = pathDiag (i+1) (j+1) ls
      | j > 7 = pathDiag (i - i+1 + (if i+1==j then 0 else -1)) (j - i+1  + (if i+1==j then 1 else 0)) (Empty : ls)
      | i > 6 = pathDiag (i - i+1) (j - i+2) (Empty : ls)
      | otherwise = pathDiag (i+1) (j+1) (a ! (i,j) : ls)
    pathDiag' i j ls
      | j == 8 && i == 5 = ls
      | i<1 || j<1 = pathDiag' (i+1) (j+1) ls
      | j > 7 = pathDiag' (i - i+1 + (if i+1==j then 0 else -1)) (j - i+1  + (if i+1==j then 1 else 0)) (Empty : ls)
      | i > 6 = pathDiag' (i - i+1) (j - i+2) (Empty : ls)
      | otherwise = pathDiag' (i+1) (j+1) (a ! (i,8-j) : ls)
    walkRes = foldr walkIt (0,0) walkPath
    walkIt _ (4,j)     = (4,j)
    walkIt _ (i,4)     = (i,4)
    walkIt Empty (i,j) = (0,0)
    walkIt X (i,_)     = (i+1,0)
    walkIt O (_,j)     = (0,j+1)


{-play :: Int -> Four -> Bool -> [Four]
play d f b = mv:play d mv (not b)
  where
    mv = move . runMemo $ minMaxAB' next end (Cutoff f 0,d,b,-100000,100000)-}

playAgainst :: Int -> Four -> Bool -> Bool -> IO ()
playAgainst d f b p
  | end f = print "Game has ended" >> return ()
  | not p = do
      let mv = runMemo $ minMaxABPrio next end (Cutoff f 0,d,b,-100000,100000)
      print $ move mv
      print $ heuristic mv
      playAgainst d (move mv) b (not p)
  | otherwise = do
      print "Enter your move"
      i <- readLn
      let mv = place f i
      print mv
      print $ heuristic mv
      playAgainst d mv b (not p)

