module Examples.Memo where

import           Util.Memoize

fib :: (Num t, Num a, Eq a) => a -> t
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' :: (Ord a, Num a) => a -> Memo a a a
fib' = memoize' h
  where
    h 0 = return 0
    h 1 = return 1
    h n = (+) <$> fib' (n-1) <*> fib' (n-2)
