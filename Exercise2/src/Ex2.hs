module Ex2 where
import Data.Maybe
import Debug.Trace
import Text.Printf

debug = flip trace

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
-- this will drop the first 143 elements of xs, then take the 144th and append it to f1 ys
-- empty list will return empty list
f1 xs = case drop (143) xs of
              y : ys -> y : f1 ys -- y : (y2 : (y3 : (y4 : (...))))
              [] -> []

-- Q2 (3 marks)
f2 :: [Int] -> Int
-- sums every 263rd element of its input
f2 ns = case drop (262) ns of
              y : ys -> y + f2 ys -- y + (y2 + (y3 + (y4 + (...))))
              [] -> 0

-- Q3 (4 marks)
f3 :: [Int] -> Int
-- multiplies every 341st element of its input
f3 ns = case drop (340) ns of
              y : ys -> y * f3 ys -- y * (y2 * (y3 * (y4 * (...))))
              [] -> 1

-- Q4 (8 marks)
f4 :: [Maybe Int] -> (Int, [Maybe Int])
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
-- -1 = term, -2 = skip, any other value will be set as the number it must be
f4 (Just x:xs)
  | x == 60 = (addRecurse (take 6 xs) (-1), (drop 6 xs)) `debug` "OPCODE: 60"
  | x == 32 = (addRecurse (take 6 xs) (-2), (drop 6 xs))
  | x == 41 = (addRecurse (take 6 xs) 9, (drop 6 xs)) `debug` "OPCODE: 41"
  | x == 71 = (addRecurse (findStop xs 6) (-1), (drop (length (findStop xs 6)) xs))
  | x == 40 = (addRecurse (findStop xs 3) (-2), (drop (length (findStop xs 3)) xs))
  | x == 68 = (addRecurse (findStop xs 4) 6, (drop (length (findStop xs 4)) xs))
  | x == 73 = (mulRecurse (take 5 xs) (-1), (drop 5 xs))
  | x == 57 = (mulRecurse (take 5 xs) (-2), (drop 5 xs))
  | x == 52 = (mulRecurse (take 6 xs) 7, (drop 6 xs))
  | x == 43 = (mulRecurse (findStop xs 4) (-1), (drop (length (findStop xs 4)) xs))
  | x == 53 = (mulRecurse (findStop xs 6) (-2), (drop (length (findStop xs 6)) xs))
  | x == 35 = (mulRecurse (findStop xs 3) 1, (drop (length (findStop xs 3)) xs))
  | otherwise = (x,xs)

-- NOTE: rest means "rest of the integer list"
-- An opcode defines three things: 
--    1.  OPERATION(add, mul),
--    2.  HOW MANY DATA VALUES(operands) TO PROCESS(fixed/stop@),
--          a.  fixed N means operate on the next N numbers.
--          b.  stop @K means stop when value K is encountered(not considered an operand).
--    3.  HOW TO HANDLE CORUPTTED VALUES(stop/skip/def-value)
--          a. stop(term/terminate) = finish calculation
--          b. skip = move to next list element
--          c. N = treat corrupted element as having value N
--    This gives 12 combinations of things to do.
--    Opcode 42(say) does "add" with "fixed 3" and "terminate":
--        f4 [42,2,4,6,rest] results in 12,rest
--        f4 [42,2,X,6,rest] results in 2,[6,rest]
--    Opcode 24(say) does "mul" with "stop@0" and "skip":
--        f4 [23,2,4,6,2,0,rest] results in 96,rest
--        f4 [23,2,X,6,2,0,rest] results in 24,rest
--        f4 [23,2,6,2,2,2] results in 96,[]
--        
--        f4, given an empty list, returns 0.
-- 

-- Q5 (2 marks)
f5 :: [Maybe Int] -> [Int]
-- uses f4 to process all the opcodes in the maybe list,
-- by repeatedly applying it to the leftover part
f5 mis = undefined

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...
-- ADDITION HELPERS
-- The second argument will be denoted as: -1 for term, -2 for skip, and any other value will become x
addRecurse :: [Maybe Int] -> Int -> Int
addRecurse [] _ = 0
addRecurse (x:xs) nothingCase
  | not(isNothing x) = (fromJust x) + (addRecurse xs nothingCase) `debug` "Recursed Normally"
  | isNothing x = 
    case nothingCase of
      (-1) -> 0 `debug` "-1"
      (-2) -> addRecurse xs nothingCase `debug` "-2"
      otherwise -> (nothingCase) + (addRecurse xs nothingCase) `debug` "Def-val"

findStop :: [Maybe Int] -> Int -> [Maybe Int]
findStop (x:xs) stopNum
  | x /= (Just stopNum) = x : findStop xs stopNum
  | otherwise = []

-- findTerminate :: [Maybe Int] -> [Maybe Int]

mulRecurse :: [Maybe Int] -> Int-> Int
mulRecurse [] _ = 1
mulRecurse (x:xs) nothingCase
  | isJust x = (fromJust x) + (addRecurse xs nothingCase)
  | otherwise = 
    case nothingCase of
      (-1) -> 1 `debug` "-1"
      (-2) -> mulRecurse xs nothingCase `debug` "-2"
      otherwise -> (nothingCase) * (mulRecurse xs nothingCase) `debug` "Def-Val"