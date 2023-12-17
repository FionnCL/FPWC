module Ex5 where
-- no code for Q1


-- for Q2:
frec x  =  if x <= 4 then 7 else x * frec (x `div` 8)



-- for Q3:
bonus []      =  10
bonus (x:xs)  =  x + 13 + bonus xs




--for Q4:
casef x
  | x < 10   =  2*x
  | x >= 10  = 2*x-1


