module Examples.Queens where

newtype QBoard = QBoard [Int] deriving (Eq,Show)

fitness :: QBoard -> Int
fitness (QBoard []) = 0
fitness (QBoard (x:xs)) = fitness (QBoard xs) + conflicts
  where
    conflicts = length . filter (\(i,y) -> y==x || i == abs (y-x)) . zip [1..] $ xs

instance Ord QBoard where
  compare a b = compare (negate $ fitness a) (negate $ fitness b)

update :: Int -> a -> [a] -> [a]
update 0 a (_:as) = a:as
update _ _ [] = []
update x a (a1:as) = a1:update (x-1) a as

switch :: Int -> Int -> QBoard -> QBoard
switch i j (QBoard xs) = QBoard . update i (xs!!j) . update j (xs!!i) $ xs

next :: QBoard -> [QBoard]
next (QBoard []) = []
next (QBoard (x:xs)) = nextI ++ ((\i -> QBoard . ((xs!!i):) $ update i x xs) <$> [0..length xs - 1])
  where
    nextI = (`append` x) <$> next (QBoard xs)
    append (QBoard xs) x = QBoard (x:xs)

test1 = QBoard [1,2,3,4]
test2 = QBoard [3,1,2,4]
