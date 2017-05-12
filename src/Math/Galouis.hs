module Math.Galouis where

data GF a = GF { prime :: a, number :: a }

instance Integral a => Num (GF a) where
  (GF p a) + (GF _ b) = GF p ((a + b) `mod` p)
  (GF p a) * (GF _ b) = GF p ((a * b) `mod` p)
  abs (GF p a) = GF p a
  signum (GF p a) = GF p a
  fromInteger x = GF 2 (fromInteger x)
  negate (GF p a) = GF p (p-a)

instance (Integral a, Fractional a) => Fractional (GF a) where
  (GF p a) / (GF _ b) = GF p ((a*inv) `mod` p)
    where
      inv = head $ filter (\c -> (c*b) `mod` p == 1) [1..(p-1)]
  fromRational a = GF 2 (fromRational a)
