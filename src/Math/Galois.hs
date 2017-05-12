{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Math.Galois where

import GHC.TypeLits

--TODO: Compare performance, test LGS solving

type GT = GFs 7 Integer
gt :: Integer -> GFs 7 Integer
gt = GFs

newtype GFs (a :: Nat) b = GFs b deriving Show

newtype ProxGF b a = P (GFs a b)

instance (KnownNat b, Integral a) => Num (GFs b a) where
  c@(GFs a) + (GFs b) = GFs ((a + b) `mod` primeV c)
  c@(GFs a) * (GFs b) = GFs ((a * b) `mod` primeV c)
  abs (GFs a) = GFs a
  signum (GFs a) = GFs a
  fromInteger a = let res = GFs (fromInteger a `mod` primeV res) in res
  negate c@(GFs a) = GFs ((primeV c - a) `mod` primeV c)

instance (KnownNat b, Integral a, Fractional a) => Fractional (GFs b a) where
  c@(GFs a) / (GFs b) = GFs ((a*inv) `mod` p)
    where
      inv = head $ filter (\c -> ((c*b) `mod` p) == 1) [1..(p-1)]
      p = primeV c
  fromRational = GFs . fromRational

instance (KnownNat b, Eq a, Integral a) => Eq (GFs b a) where
  c@(GFs a) == (GFs b) = a `mod` p == b `mod` p
    where
      p = primeV c

instance (KnownNat b, Enum a, Integral a) => Enum (GFs b a) where
  toEnum a = let res = GFs (toEnum a `mod` primeV res) in res
  fromEnum (GFs a) = fromEnum a

instance (Real a, KnownNat b, Integral a) => Ord (GFs b a) where
  compare c@(GFs a) (GFs b) = compare (a `mod` p) (b `mod` p)
    where
      p = primeV c

instance (Real a, KnownNat b, Integral a) => Real (GFs b a) where
  toRational (GFs a) = toRational a

instance (Real a, KnownNat b, Integral a) => Integral (GFs b a) where
  toInteger (GFs a) = toInteger a
  divMod c@(GFs a) (GFs b) = (GFs ((a*inv b) `mod` p),GFs 0)
    where
      inv b = head $ filter (\c -> ((c*b) `mod` p) == 1) [1..(p-1)]
      p = primeV c
  quotRem c@(GFs a) (GFs b) = (GFs ((a*inv b) `mod` p),GFs 0)
    where
      inv b = head $ filter (\c -> ((c*b) `mod` p) == 1) [1..(p-1)]
      p = primeV c

primeV :: (KnownNat b, Integral a) => GFs b a -> a
primeV = fromInteger . natVal . P
