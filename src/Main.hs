module Main where

import           Criterion
import           Criterion.Main
import           Data.Time
import           Examples.Memo
import           Examples.Tic
import           Search.Adversarial
import           Util.Memoize

main = do
  print . childs . runMemo $ minMaxAB' next end (Cutoff start 0,1000000,True,-10000,10000)
{-  print . childs $ minMaxAB (Cutoff start 0) 1000000 next end True (-10000) 10000-}


memo n = childs . runMemo $ minMaxAB' next end (Cutoff start 0,1000000+n-n,True,-10000,10000)

prio n = childs . runMemo $ minMaxABPrio next end (Cutoff start 0,1000000+n-n,True,-10000,10000)

ab n = childs $ minMaxAB (Cutoff start 0) (1000000+n-n) next end True (-10000) 10000

{-main = defaultMain [
  bgroup "fib" [ bench "Memo"  $ whnf memo 1
               , bench "Prio" $ whnf prio 1
               , bench "AlphaBeta"  $ whnf ab 5
               ]
  ]-}
