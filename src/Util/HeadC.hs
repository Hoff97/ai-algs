{-# LANGUAGE MultiParamTypeClasses #-}

module Util.HeadC where

import           Data.PQueue.Prio.Max as PQ

class HeadC f where
  headC :: (f a) -> Maybe a
  tailC :: (f a) -> Maybe (f a)

instance HeadC [] where
  headC []    = Nothing
  headC (x:_) = Just x

  tailC []     = Nothing
  tailC (_:xs) = Just xs

instance Ord k => HeadC (MaxPQueue k) where
  headC = (fst <$>) . maxView

  tailC = (snd <$>) . maxView
