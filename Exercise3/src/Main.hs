module Main where

import Ex3

d = [("XX",1),("XD",20),("DD", 41)]

main
  = putStrLn $ unlines
      [ "Running Exercise3."
      , "To run interpreter use 'stack ghci src/Ex3.hs'."
      , "You can add further strings of output here if it helps."
      , "\"eval\" Tests"
      , "eval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (MulBy (VarNm \"XD\") (Value 3)) = " ++ show (eval d (MulBy (VarNm "XD") (Value 3.0)))
      , "eval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Divide (Value 1) (Value 3)) = " ++ show (eval d (Divide (Value 1.0) (Value 3.0)))
      , "eval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Divide (Value 1) (Value 0)) = " ++ show (eval d (Divide (Value 1.0) (Value 0.0)))
      , "eval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (AddInv (Value 1)) = " ++ show (eval d (AddInv (Value 1.0)))
      , "eval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (AddInv (Value -1)) = " ++ show (eval d (AddInv (Value (-1.0))))
      , "eval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Not (Value 0)) = " ++ show (eval d (Not (Value 0.0)))
      , "eval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Not (Value 1)) = " ++ show (eval d (Not (Value 1.0)))
      , "eval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Dfrnt (Value 1) (Value 3)) = " ++ show (eval d (Dfrnt (Value 1.0) (Value 3.0)))
      , "eval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Dfrnt (Value 1) (Value 1)) = " ++ show (eval d (Dfrnt (Value 1.0) (Value 1.0)))
      , "eval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (IsNil (Value 0.0)) = " ++ show (eval d (IsNil (Value 0.0)))
      , "eval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (IsNil (Value 1.0)) = " ++ show (eval d (IsNil (Value 1.0)))

      , "\"meval\" Tests"
      , "meval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (MulBy (VarNm \"XD\") (Value 3)) = " ++ show (meval d (MulBy (VarNm "XD") (Value 3.0)))
      , "meval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Divide (Value 1) (Value 3)) = " ++ show (meval d (Divide (Value 1.0) (Value 3.0)))
      , "meval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Divide (Value 1) (Value 0)) = " ++ show (meval d (Divide (Value 1.0) (Value 0.0)))
      , "meval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (AddInv (Value 1)) = " ++ show (meval d (AddInv (Value 1.0)))
      , "meval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (AddInv (Value -1)) = " ++ show (meval d (AddInv (Value (-1.0))))
      , "meval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Not (Value 0)) = " ++ show (meval d (Not (Value 0.0)))
      , "meval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Not (Value 1)) = " ++ show (meval d (Not (Value 1.0)))
      , "meval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Dfrnt (Value 1) (Value 3)) = " ++ show (meval d (Dfrnt (Value 1.0) (Value 3.0)))
      , "meval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (Dfrnt (Value 1) (Value 1)) = " ++ show (meval d (Dfrnt (Value 1.0) (Value 1.0)))
      , "meval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (IsNil (Value 0.0)) = " ++ show (meval d (IsNil (Value 0.0)))
      , "meval [(\"XX\",1),(\"XD\",20),(\"DD\", 41)] (IsNil (Value 1.0)) = " ++ show (meval d (IsNil (Value 1.0)))
      , "meval [(\"X\",1)] (MulBy (VarNm \"B\") (Value 3)) = " ++ show (meval [("X",1)] (MulBy (VarNm "XD") (Value 3.0)))
      , "meval [] (MulBy (VarNm \"B\") (Value 3)) = " ++ show (meval [] (MulBy (VarNm "XD") (Value 3.0)))
      
      , "\"simp\" Tests"
      , "simp (MulBy 20 0) = " ++ show (simp (MulBy (Value 20) (Value 0)))
      , "simp (MulBy 0 20) = " ++ show (simp (MulBy (Value 0) (Value 20)))
      , "simp (MulBy 20 1) = " ++ show (simp (MulBy (Value 20) (Value 1)))
      , "simp (MulBy 1 20) = " ++ show (simp (MulBy (Value 1) (Value 20)))
      , "simp (MulBy 2 20) = " ++ show (simp (MulBy (Value 2) (Value 20)))
      ]
