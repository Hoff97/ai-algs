{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.Memoize where

import           Control.Monad.Identity
import           Control.Monad.State.Lazy
import           Data.Hashable
import           Data.HashMap.Strict      as HM
import           Data.Map.Lazy            as M
import           Debug.Trace

newtype MemoT a b m c = MemoT (StateT (HashMap a b) m c) deriving (Functor, Applicative, Monad)

type Memo a b c = MemoT a b Identity c

unMemo :: MemoT a b m c -> StateT (HashMap a b) m c
unMemo (MemoT s) = s

memoize :: (Eq a, Hashable a, Monad m) => (a -> b) -> a -> MemoT a b m b
memoize f a = MemoT $ do
  m <- get
  case HM.lookup a m of
    Just r  -> return r
    Nothing -> do
      let r = f a
      modify (HM.insert a r)
      return r

memoize' :: (Eq a, Hashable a, Monad m) => (c -> a) -> (c -> b) -> c -> MemoT a b m b
memoize' t f c = MemoT $ do
  let a = t c
  m <- get
  case HM.lookup a m of
    Just r  -> return r
    Nothing -> do
      let r = f c
      modify (HM.insert a r)
      return r

memoizeRec :: (Eq a, Hashable a, Monad m) => (a -> MemoT a b m b) -> a -> MemoT a b m b
memoizeRec f a = MemoT $ do
  m <- get
  case HM.lookup a m of
    Just r  -> return r
    Nothing -> do
      r <- unMemo $ f a
      modify (HM.insert a r)
      return r

memoizeRec' :: (Eq a, Hashable a, Monad m) => (c -> a) -> (c -> MemoT a b m b) -> c -> MemoT a b m b
memoizeRec' t f c = MemoT $ do
  let a = t c
  m <- get
  case HM.lookup a m of
    Just r  -> return r
    Nothing -> do
      r <- unMemo $ f c
      modify (HM.insert a r)
      return r

memoizeChange :: (Eq a, Hashable a, Monad m) => (a -> Maybe b -> MemoT a b m b) -> a -> MemoT a b m b
memoizeChange f a = MemoT $ do
  m <- get
  result <- unMemo $ f a (HM.lookup a m)
  modify (HM.insert a result)
  return result

memoizeChange' :: (Eq a, Hashable a, Monad m) => (c -> a) -> (c -> Maybe b -> MemoT a b m b) -> c -> MemoT a b m b
memoizeChange' t f c = MemoT $ do
  let a = t c
  m <- get
  result <- case HM.lookup a m of
    Just r  -> unMemo $ f c (Just r)
    Nothing -> unMemo $ f c Nothing
  modify (HM.insert a result)
  return result

runMemoT :: Functor f => MemoT a b f c -> f c
runMemoT (MemoT t) = fst <$> runStateT t HM.empty

runMemo :: MemoT a b Identity c -> c
runMemo = runIdentity . runMemoT
