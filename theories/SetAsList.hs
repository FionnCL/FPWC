module SetAsList where


ins x [] = [x]               -- ins.1
ins x (y:ys)                 -- ins.2
  | x < y   =  x:y:ys        -- ins.2.1
  | x > y   =  y : ins x ys  -- ins.2.2
  | x == y  =  y:ys          -- ins.2.3

mbr _ [] =  False        -- mbr.1
mbr x (y:ys)             -- mbr.2
  | x < y   =  False     -- mbr.2.1
  | x > y   =  mbr x ys  -- mbr.2.2
  | x == y  =  True      -- mbr.2.3
  