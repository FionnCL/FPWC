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
