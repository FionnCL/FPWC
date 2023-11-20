module Main where

import Ex4

main
  = putStrLn $ unlines
      [ "Running Exercise4."
      , "You should modify this program as follows:"
      , "It should open and read a file called `input.dat`"
      , "This file contains a number of Ints, each on its own line"
      , "There is a list of functions defined in variable `ops` in Ex4.hs"
      , "Your `ops` list has length N="++show (length ops)
      , "The 1st function is applied to the 1st number read,"
      , "The 2nd function is applied to the 2nd number read,"
      , " proceed like this until:"
      , "The Nth function is applied to the Nth number read."
      , "Processing moves back to the 1st function in the list, so..."
      , "The 1st function is applied to the (N+1)th number read,"
      , "The 2st function is applied to the (N+2)th number read,"
      , "and so on..."
      , "Continue until all input numbers have been processed."
      , "The resulting numbers should be written, one per line, to `output.dat`"
      ]
