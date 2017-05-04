module Util.Color where

hsvToRGB :: (Float,Float,Float) -> (Float,Float,Float)
hsvToRGB (h,s,v)
  | hi == 0 || hi == 6 = (v,t,p)
  | hi == 1 = (q,v,p)
  | hi == 2 = (p,v,t)
  | hi == 3 = (p,q,v)
  | hi == 4 = (t,p,v)
  | otherwise = (v,p,q)
  where
    hi = floor (h/60)
    f = h/60 - fromIntegral hi
    p = v*(1-s)
    q = v*(1-s*f)
    t = v*(1-s*(1-f))
