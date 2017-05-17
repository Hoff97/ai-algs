module Util.QueueTest where

import Util.Queue
import Test.QuickCheck ((==>),Property, quickCheck,Arbitrary,arbitrary,shrink)
import Data.Maybe (isNothing)

instance Arbitrary a => Arbitrary (Queue a) where
  arbitrary = Queue <$> arbitrary <*> arbitrary
  shrink (Queue as bs) = [Queue as [],Queue [] bs] ++ [Queue as' bs' | (as',bs') <- shrink (as,bs)]

queueTest = do
  quickCheck pushPop
  quickCheck pushList
  quickCheck pushLists
  quickCheck popEmpty

pushPop :: Int -> Bool
pushPop a = (==Just (a,emptyQ)) . pop . push a $ emptyQ

pushList :: [Int] -> Property
pushList l = length l > 0 ==>
  (==l) . popAll . pushAll l $ emptyQ

pushLists :: [[Int]] -> Bool
pushLists l = (==concat (reverse l)) . popAll . foldr pushAll emptyQ $ l

popEmpty :: Bool
popEmpty = isNothing $ pop (emptyQ :: Queue Int)
