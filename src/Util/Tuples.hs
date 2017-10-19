{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Util.Tuples where

class First a where
  type Fst a
  type NFst a

  first :: a -> Fst a
  dropFirst :: a -> NFst a

instance First (a,b) where
  type Fst (a,b) = a
  type NFst (a,b) = b
  first (a,_) = a
  dropFirst (_,b) = b

instance First (a,b,c) where
  type Fst (a,b,c) = a
  type NFst (a,b,c) = (b,c)
  first (a,_,_) = a
  dropFirst (_,b,c) = (b,c)

instance First (a,b,c,d) where
  type Fst (a,b,c,d) = a
  type NFst (a,b,c,d) = (b,c,d)
  first (a,_,_,_) = a
  dropFirst (_,b,c,d) = (b,c,d)

instance First (a,b,c,d,e) where
  type Fst (a,b,c,d,e) = a
  type NFst (a,b,c,d,e) = (b,c,d,e)
  first (a,_,_,_,_) = a
  dropFirst (_,b,c,d,e) = (b,c,d,e)

second :: (First a, First (NFst a)) => a -> Fst (NFst a)
second = first . dropFirst

third :: (First a, First (NFst a), First (NFst (NFst a))) => a -> Fst (NFst (NFst a))
third = first . dropFirst . dropFirst

fourth :: (First a, First (NFst a), First (NFst (NFst a)), First (NFst (NFst (NFst a)))) => a -> Fst (NFst (NFst (NFst a)))
fourth = first . dropFirst . dropFirst . dropFirst

fifth :: (First a, First (NFst a), First (NFst (NFst a)), First (NFst (NFst (NFst a))), First (NFst (NFst (NFst (NFst a))))) => a -> Fst (NFst (NFst (NFst (NFst a))))
fifth = first . dropFirst . dropFirst . dropFirst . dropFirst
