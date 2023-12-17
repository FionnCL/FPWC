module Factorial where
  
iffac n = if n==0 then 1 else n * iffac (n-1)

gfac n
  | n == 0  =  1
  | otherwise  =  n * gfac (n-1)

pfac 0  =  1
pfac n  =  n * pfac (n-1)
