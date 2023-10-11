module Ex2 where

add :: Int -> Int -> Int
add x y = (x+y) `mod` 65563

mul :: Int -> Int -> Int
mul x y
  | p == 0    = 1
  | otherwise = p
  where p = (x*y) `mod` 65563

-- DON'T RENAME THE SPECIFIED FUNCTIONS (f1..fN)
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (3 marks)
f1 :: [a] -> [a]
-- returns a list of every 144th element of its input
-- ANALYSE THIS AND SEE HOW IT WORKS!
f1 xs = case drop (143) xs of
              y : ys -> y : f1 ys
              [] -> []

-- Q2 (3 marks)
f2 :: [Int] -> Int
-- sums every 263rd element of its input
f2 ns = case drop (262) ns of
              y : ys -> y + f2 ys: f2 ys
              [] -> 0

-- Q3 (4 marks)
f3 :: [Int] -> Int
-- multiplies every 341st element of its input
f3 ns = undefined

-- Q4 (8 marks)
f4 :: [Maybe Int] -> (Int,[Maybe Int])
-- Operation Table (See Exercise2 description on BB)
--    ___________________________________________
--    | opcode | operation | operands | Nothing |
--    -------------------------------------------
--    |   60   |    add    | fixed 6  | term    |
--    |   32   |    add    | fixed 6  | skip    |
--    |   41   |    add    | fixed 6  | 9       |
--    |   71   |    add    | stop@ 6  | term    |
--    |   40   |    add    | stop@ 3  | skip    |
--    |   68   |    add    | stop@ 4  | 6       |
--    |   73   |    mul    | fixed 5  | term    |
--    |   57   |    mul    | fixed 5  | skip    |
--    |   52   |    mul    | fixed 6  | 7       |
--    |   43   |    mul    | stop@ 4  | term    |
--    |   53   |    mul    | stop@ 6  | skip    |
--    |   35   |    mul    | stop@ 3  | 1       |
--    -------------------------------------------
f4 mis = undefined

-- Q5 (2 marks)
f5 :: [Maybe Int] -> [Int]
-- uses f4 to process all the opcodes in the maybe list,
-- by repeatedly applying it to the leftover part
f5 mis = undefined

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

