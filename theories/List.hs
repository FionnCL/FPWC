module List where

[] ++ ys      =  ys               -- ++.1
(x:xs) ++ ys  =  x:(xs ++ ys)     -- ++.2

rev []        =  []               -- rev.1
rev (x:xs)    =  rev xs ++ [x]    -- rev.2

len []        =  0                -- len.1
len (_:xs)    =  1 + len xs       -- len.2

sum []        =  0                -- sum.1
sum (x:xs)    =  x + sum xs       -- sum.2

prd []        =  1                -- prd.1
prd (x:xs)    =  x * prd xs       -- prd.2

rpl 0 _       =  []               -- rpl.1
rpl n x       =  x : rpl (n-1) x  -- rpl.2
