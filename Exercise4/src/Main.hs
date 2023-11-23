module Main where

import Ex4
import System.IO

d = [("XX",1),("XD",20),("DD", 41)]


doOperations :: [(Integer -> Integer)] -> [Integer] -> [Integer]
doOperations _ [] = []
doOperations (op:ops) (x:xs) = (op x) : doOperations ops xs

writeList :: Handle -> String -> IO ()
writeList _ [] = return ()
writeList h (x:xs)
 = do hPutChar h x
      writeList h xs

toString :: (Show a) => [a] -> String
toString [] = ""
toString [x] = (show x)
toString (x:xs) = (show x) ++ "\n" ++ toString xs

main = do
      h <- openFile "./input.dat" ReadMode
      ho <- openFile "./output.dat" WriteMode
      contents <- hGetContents h
      let l = map read (lines contents)
      let opsList = take (length l) (cycle ops)
      let list = doOperations opsList l
      let result = toString list
      print result
      writeList ho result
      hClose h
      hClose ho
      
-- main 
--   = putStrLn $ unlines
--       [ "Running Exercise4."
--       , "You should modify this program as follows:"
--       , "It should open and read a file called `input.dat`"
--       , "This file contains a number of Ints, each on its own line"
--       , "There is a list of functions defined in variable `ops` in Ex4.hs"
--       , "Your `ops` list has length N="++show (length ops)
--       , "The 1st function is applied to the 1st number read,"
--       , "The 2nd function is applied to the 2nd number read,"
--       , " proceed like this until:"
--       , "The Nth function is applied to the Nth number read."
--       , "Processing moves back to the 1st function in the list, so..."
--       , "The 1st function is applied to the (N+1)th number read,"
--       , "The 2st function is applied to the (N+2)th number read,"
--       , "and so on..."
--       , "Continue until all input numbers have been processed."
--       , "The resulting numbers should be written, one per line, to `output.dat`"

--       , "\"mdeval\" Tests"
--       , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (MulBy (VarNm \"XD\") (Value 3)) = " ++ show (mdeval d (MulBy (VarNm "XD") (Value 3.0)))
--       , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Divide (Value 1) (Value 3)) = " ++ show (mdeval d (Divide (Value 1.0) (Value 3.0)))
--       , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Divide (Value 1) (Value 0)) = " ++ show (mdeval d (Divide (Value 1.0) (Value 0.0)))
--       , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (AddInv (Value 1)) = " ++ show (mdeval d (AddInv (Value 1.0)))
--       , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (AddInv (Value -1)) = " ++ show (mdeval d (AddInv (Value (-1.0))))
--       , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Not (Value 0)) = " ++ show (mdeval d (Not (Value 0.0)))
--       , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Not (Value 1)) = " ++ show (mdeval d (Not (Value 1.0)))
--       , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Dfrnt (Value 1) (Value 3)) = " ++ show (mdeval d (Dfrnt (Value 1.0) (Value 3.0)))
--       , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Dfrnt (Value 1) (Value 1)) = " ++ show (mdeval d (Dfrnt (Value 1.0) (Value 1.0)))
--       , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (IsNil (Value 0.0)) = " ++ show (mdeval d (IsNil (Value 0.0)))
--       , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (IsNil (Value 1.0)) = " ++ show (mdeval d (IsNil (Value 1.0)))
--       ]