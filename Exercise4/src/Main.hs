module Main where

import Ex4
import System.IO
--import qualified Data.Text    as Text
--import qualified Data.Text.IO as Text

d = [("XX",1),("XD",20),("DD", 41)]

writeWholeFile :: Handle -> String -> IO ()
writeWholeFile _ [] = return ()
writeWholeFile h (x:xs)
 = do hPutChar h x
      writeWholeFile h xs

readWholeFile :: Handle -> IO String
readWholeFile h
 = do eof <- hIsEOF h
      if eof then return []
              else do c <- hGetChar h
                      str <- readWholeFile h
                      return (c:str)



-- fCopyAllChars :: [Int] -> [Int] -> [Int] -> FilePath -> IO ()
-- fCopyAllChars savedxs (x:xs) (op:ops) tof
--   = do  tf <- openFile tof WriteMode
--         result <- fCopyAllChars savedxd xs ops tof
--         writeWholeFile tf (result)
--         hClose tf


-- fCopyAllChars "./input.dat" "./output.dat"

main
     = do
        --ls <- fmap Text.lines (Text.readFile "./input.dat")
        ls <- "XD"
        print ls
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

      -- , "\"mdeval\" Tests"
      -- , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (MulBy (VarNm \"XD\") (Value 3)) = " ++ show (mdeval d (MulBy (VarNm "XD") (Value 3.0)))
      -- , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Divide (Value 1) (Value 3)) = " ++ show (mdeval d (Divide (Value 1.0) (Value 3.0)))
      -- , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Divide (Value 1) (Value 0)) = " ++ show (mdeval d (Divide (Value 1.0) (Value 0.0)))
      -- , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (AddInv (Value 1)) = " ++ show (mdeval d (AddInv (Value 1.0)))
      -- , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (AddInv (Value -1)) = " ++ show (mdeval d (AddInv (Value (-1.0))))
      -- , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Not (Value 0)) = " ++ show (mdeval d (Not (Value 0.0)))
      -- , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Not (Value 1)) = " ++ show (mdeval d (Not (Value 1.0)))
      -- , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Dfrnt (Value 1) (Value 3)) = " ++ show (mdeval d (Dfrnt (Value 1.0) (Value 3.0)))
      -- , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Dfrnt (Value 1) (Value 1)) = " ++ show (mdeval d (Dfrnt (Value 1.0) (Value 1.0)))
      -- , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (IsNil (Value 0.0)) = " ++ show (mdeval d (IsNil (Value 0.0)))
      -- , "mdeval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (IsNil (Value 1.0)) = " ++ show (mdeval d (IsNil (Value 1.0)))
--       ]
