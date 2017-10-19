{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.Memoize where

import           Control.Monad.Identity
import           Control.Monad.State.Lazy
import           Data.Map.Lazy            as M

newtype MemoT a b m c = MemoT (StateT (M.Map a b) m c) deriving (Functor, Applicative, Monad)

type Memo a b c = MemoT a b Identity c

unMemo :: MemoT a b m c -> StateT (M.Map a b) m c
unMemo (MemoT s) = s

memoize :: (Ord a, Monad m) => (a -> b) -> a -> MemoT a b m b
memoize f a = MemoT $ do
  m <- get
  case M.lookup a m of
    Just r  -> return r
    Nothing -> do
      let r = f a
      modify (insert a r)
      return r

memoize' :: (Ord a, Monad m) => (c -> a) -> (c -> b) -> c -> MemoT a b m b
memoize' t f c = MemoT $ do
  let a = t c
  m <- get
  case M.lookup a m of
    Just r  -> return r
    Nothing -> do
      let r = f c
      modify (insert a r)
      return r

memoizeRec :: (Ord a, Monad m) => (a -> MemoT a b m b) -> a -> MemoT a b m b
memoizeRec f a = MemoT $ do
  m <- get
  case M.lookup a m of
    Just r  -> return r
    Nothing -> do
      r <- unMemo $ f a
      modify (insert a r)
      return r

memoizeRec' :: (Ord a, Monad m) => (c -> a) -> (c -> MemoT a b m b) -> c -> MemoT a b m b
memoizeRec' t f c = MemoT $ do
  let a = t c
  m <- get
  case M.lookup a m of
    Just r  -> return r
    Nothing -> do
      r <- unMemo $ f c
      modify (insert a r)
      return r

memoizeChange :: (Ord a, Monad m) => (a -> Maybe b -> MemoT a b m b) -> a -> MemoT a b m b
memoizeChange f a = MemoT $ do
  m <- get
  result <- unMemo $ f a (M.lookup a m)
  modify (M.insert a result)
  return result

memoizeChange' :: (Ord a, Monad m) => (c -> a) -> (c -> Maybe b -> MemoT a b m b) -> c -> MemoT a b m b
memoizeChange' t f c = MemoT $ do
  let a = t c
  m <- get
  result <- unMemo $ f c (M.lookup a m)
  modify (M.insert a result)
  return result

runMemoT :: Functor f => MemoT a b f c -> f c
runMemoT (MemoT t) = fst <$> runStateT t empty

runMemo :: MemoT a b Identity c -> c
runMemo = runIdentity . runMemoT
