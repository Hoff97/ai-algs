module Main where

import           Criterion
import           Criterion.Main
import           Data.Time
import           Examples.Memo
import           Examples.Tic
import           Search.Adversarial
import           Util.Memoize

main = do
  print . childs . runMemo $ minMaxABPrio next end (Cutoff start 0,125000,True,-10000,10000)
{-print . childs . runMemo $ minMaxABPrio next end (Cutoff start 0,1000000,True,-10000,10000)-}


memo m n = childs . runMemo $ minMaxAB' next end (Cutoff start 0,m+n-n,True,-10000,10000)

prio m n = childs . runMemo $ minMaxABPrio next end (Cutoff start 0,m+n-n,True,-10000,10000)

ab m n = childs $ minMaxAB (Cutoff start 0) (m+n-n) next end True (-10000) 10000

{-main = defaultMain [
  bgroup "prio" [ bench "6" $ whnf (prio 200000) 1
                , bench "5.5" $ whnf (prio 150000) 1
                , bench "5.25" $ whnf (prio 125000) 1
                , bench "5.125" $ whnf (prio 112000) 1
                , bench "5.0125" $ whnf (prio 101200) 1
                , bench "5" $ whnf (prio 100000) 1
                , bench "4.75" $ whnf (prio 88000) 1
                , bench "4.5" $ whnf (prio 80000) 1
                , bench "4.25" $ whnf (prio 75000) 1
               ],
  bgroup "memo" [bench "6" $ whnf (memo 200000) 1
                , bench "5.5" $ whnf (memo 150000) 1
                , bench "5.25" $ whnf (memo 125000) 1
                , bench "5.125" $ whnf (memo 112000) 1
                , bench "5.0125" $ whnf (memo 101200) 1
                , bench "5" $ whnf (memo 100000) 1
                , bench "4.75" $ whnf (memo 88000) 1
                , bench "4.5" $ whnf (memo 80000) 1
                , bench "4.25" $ whnf (memo 75000) 1
               ]
  ]-}
