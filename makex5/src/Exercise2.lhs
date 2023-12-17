\subsection{Exercise Two}

\begin{code}
module 
  Exercise2( 
    theExerciseNumber, exerciseFolder, testFolder
  , generateExercise2, generateTests2 
  ) 
where

import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO
import System.Random
import Control.Monad
import Data.List
import Data.Char
import Data.YAML as Y
import Data.Loc
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Lazy.UTF8 as BU

import Utilities
import Configuration
import Identity
import Randomisation
import RandomExercises
import TopLevel
import GenWrapper
\end{code}

\begin{code}
theExerciseNumber = (2 :: Int)
exerciseFolder = "Exercise"++show theExerciseNumber
testFolder = "Test"++show theExerciseNumber
\end{code}

\newpage
\subsubsection{Exercise Generation}

\input{exercises/Exercise-2-Plan}

\begin{code}
generateExercise2 :: String -> Config -> IO ()
generateExercise2 folder config
  = do putStr $ showConfig config
       putStrLn "Generating EX2"
       let sg0 = mkStdGen $ read $ idno config
       sg <- genEx2 folder config sg0
       return ()
\end{code}


\begin{code}
genEx2 :: String -> Config -> StdGen -> IO StdGen
genEx2 folder config sg 
  = do ([[i],[j],[k],ell],sg) <- genExercise2RNs sg

       putStrLn ("Ex2 Q1 ("++show ex2q1mark++") i = "++show i)
       q1 <- specifyQ1 mname i
       putStrLn ("Ex2 Q2 ("++show ex2q2mark++") j = "++show j)
       q2 <- specifyQ2 mname j
       putStrLn ("Ex2 Q3 ("++show ex2q3mark++") k = "++show k)
       q3 <- specifyQ3 mname k
       putStrLn ("Ex2 Q4 ("++show ex2q4mark++") ell = "++show ell)
       q4 <- specifyQ4 mname ell
       putStrLn ("Ex2 Q5 ("++show ex2q5mark++") -- no randomness")
       q5 <- specifyQ5 mname
       putStrLn ("Ex2 - Total Marks: "++show ex2marks)
      
       let supplied = suppliedQ4 ell
       let preamble = exercisePreamble supplied theExerciseNumber 0
       let qs = concat $ intersperse "\n" [q1,q2,q3,q4,q5]
       writeFile fname (preamble 
                        ++ qs 
                        ++ exercisePostamble theExerciseNumber 0)
       return sg
  where
    nn = show $ exno config
    mname = "Ex"++nn
    fname = folder </> "src" </> mname <.> "hs"

ex2q1mark = 3
ex2q2mark = 3
ex2q3mark = 4
ex2q4mark = 8
ex2q5mark = 2
ex2marks = sum [ex2q1mark,ex2q2mark,ex2q3mark,ex2q4mark,ex2q5mark]
\end{code}

\newpage
\subsubsection{Test Generation}

\begin{code}
generateTests2 :: Config -> IO ()
generateTests2 config
  = do putStr $ showConfig config
       putStrLn "Generating T2"
       let sg0 = mkStdGen $ read $ idno config
       sg <- genTst2 config sg0
       return ()
\end{code}


\begin{code}
genTst2 :: Config -> StdGen -> IO StdGen
genTst2 config sg 
  = do ([[i],[j],[k],ell],sg) <- genExercise2RNs sg

       putStrLn ("Ex2 T1 i = "++show i)
       (suppt1,t1) <- specifyT1 mname i
       putStrLn ("Ex2 T2 j = "++show j)
       (suppt2,t2) <- specifyT2 mname j
       putStrLn ("Ex2 T3 k = "++show k)
       (suppt3,t3) <- specifyT3 mname k
       putStrLn ("Ex2 T4 ell = "++show ell)
       (suppt4,t4) <- specifyT4 mname ell
       putStrLn "Ex2 T5 -- no randomness"
       (suppt5,t5) <- specifyT5 mname

       let support = concat $ intersperse "\n" 
                       [suppt1,suppt2,suppt3,suppt4,suppt5]
       let tests = concat $ intersperse "\n" [t1,t2,t3,t4,t5]
       writeFile fname 
         ( specPreamble 
              "import Test2Support" theExerciseNumber 
           ++ specBody theExerciseNumber 5
           ++ specPostamble
           ++ tests
           ++ support 
         )
       return sg
  where
    nn = show $ exno config
    mname = "Spec"
    fname = testFolder </> "test" </> mname <.> "hs"
\end{code}

\newpage
\subsubsection{Ex 2 Q 1}

\ExTwoQI

\begin{code}
specifyQ1 :: String -> Int -> IO String
specifyQ1 mname i 
  = return $ unlines $
      [ ("-- Q1 ("++show ex2q1mark++" marks)")
      , "f1 :: [a] -> [a]"
      , "-- returns a list of every "++showth i++" element of its input"
      , "f1 xs = undefined"
      ]
\end{code}

\begin{code}
specifyT1 :: String -> Int -> IO (String, String)
specifyT1 mname i 
  = return 
     ( supportT1 i
     , 
       unlines 
        [ testGroupName theExerciseNumber 1
          ++ " = testGroup \"\\nQ1 ("++show ex2q1mark++" marks)\" ["
        , "    testCase \"Q1T1 (1..i-1)  [1 mark]\""
        , "      ( f1 [1.." ++ istr ++ "-1] @?= [])"
        , "  , testCase \"Q1T2 (1..i)  [1 mark]\" "
        , "      ( f1 [1.." ++ istr ++ "] @?= [" ++ istr ++ "])"
        , "  , testCase \"Q1T3 (1..1000) [1 mark]\" "
        , "      ( f1 [1..1000] @?= collect "++istr++" [1..1000])"
        , "  ]"
        ] 
      )
      where istr = show i

supportT1 :: Int -> String
supportT1 i = unlines 
  [ "-- T1 support contained in Test2Support"
  ]
\end{code}


\newpage
\subsubsection{Ex 2 Q 2}

\ExTwoQII

\begin{code}
specifyQ2 :: String -> Int -> IO String
specifyQ2 mname j 
  = return $ unlines $
      [ ("-- Q2 ("++show ex2q2mark++" marks)")
      , "f2 :: [Int] -> Int"
      , "-- sums every "++showth j++" element of its input"
      , "f2 ns = undefined"
      ]
\end{code}

\begin{code}
specifyT2 :: String -> Int -> IO (String, String)
specifyT2 mname j 
  = return 
     ( supportT2 j
     , 
       unlines 
        [ testGroupName theExerciseNumber 2
          ++ " = testGroup \"\\nQ2 ("++show ex2q2mark++" marks)\" ["
        , "    testCase \"Q2T1 nil [1 mark]\" "
        , "      ( f2 [] @?= (addup "++jstr++" []) )"
        , "  , testCase \"Q2T2 (0..j-1) [1 mark]\" "
        , "      ( f2 [0.."++jstr++"] @?= ("++jstr++"-1) )"
        , "  , testCase \"Q2T3 (1..1000) [1 mark]\" "
        , "      ( f2 addtest @?= (addup "++jstr++" addtest) )"
        , "  ]"
        ] 
      ) where jstr = show j

supportT2 :: Int -> String
supportT2 j = unlines 
  [ "-- T2 uses T1 support"
  ]
\end{code}

\subsubsection{Ex 2 Q 3}

\ExTwoQIII

\begin{code}
specifyQ3 :: String -> Int -> IO String
specifyQ3 mname k 
  = return $ unlines $
      [ ("-- Q3 ("++show ex2q3mark++" marks)")
      , "f3 :: [Int] -> Int"
      , "-- multiplies every "++showth k++" element of its input"
      , "f3 ns = undefined"
      ]
\end{code}

\begin{code}
specifyT3 :: String -> Int -> IO (String, String)
specifyT3 mname k 
  = return 
     ( supportT3 k
     , 
       unlines 
        [ testGroupName theExerciseNumber 3
          ++ " = testGroup \"\\nQ3 ("++show ex2q3mark++" marks)\" ["
        , "    testCase \"Q3T1 nil [1 mark]\" "
        , "      ( f3 [] @?= (mulout "++kstr++" []) )"
        , "  , testCase \"Q3T2 (0..k-1) [1 mark]\" "
        , "      ( f3 [0.."++kstr++"] @?= ("++kstr++"-1) )"
        , "  , testCase \"Q3T3 (25*1..25*40) [1 mark]\" "
        , "      ( f3 multest @?= (mulout "++kstr++" multest) )"
        , "  ]"
        ] 
      ) where kstr = show k

supportT3 :: Int -> String
supportT3 k = unlines 
  [ "-- T3 uses T1 support"
  ]
\end{code}

\newpage
\subsubsection{Ex 2 Q 4}

\ExTwoQIV

Plan:  \fn{f4 :: [Maybe Int] -> (Int,[Maybe Int])}

The list is meant to contain \fn{Int}s,
but we assume some may be corrupted (\fn{Nothing}).

If the first element is not a valid opcode, it is skipped.
Different opcodes define the operand list in different ways.
Some take a fixed number of inputs, some stop when a particular value is seen.
The handling of any \fn{Nothing}s in the list differs for each opcode.
Some treat it as a terminator, some just skip over it,
and others interpert it as a particular value.

We generate a table of the form:

\begin{tabular}{|c|c|c|c|}
\hline
opcode & operation & operands & \fn{Nothing}
\\\hline 23 & \fn{add} & fixed 3 & stop 
\\\hline 34 & \fn{add} & fixed 4 & skip 
\\\hline 45 & \fn{add} & fixed 5 & 99 
\\\hline 32 & \fn{add} & stop 100 & stop 
\\\hline 43 & \fn{add} & stop 101 & skip 
\\\hline 54 & \fn{add} & stop 102 & 77 
\\\hline 56 & \fn{mul} & fixed 6 & stop 
\\\hline 67 & \fn{mul} & fixed 3 & skip 
\\\hline 78 & \fn{mul} & fixed 4 & 88 
\\\hline 65 & \fn{mul} & stop 103 & stop 
\\\hline 76 & \fn{mul} & stop 105 & skip 
\\\hline 87 & \fn{mul} & stop 105 & 66 
\\\hline
\end{tabular}~

Edge cases:
\begin{itemize}
\item At the start, non-opcode values are skipped.
\item If no opcode is found then \fn{(0,[])} is returned.
\item Given an opcode:
  \begin{itemize}
    \item If no numbers are found, then \fn{(opid,[])} is returned,
          where \fn{opid} is the unit value for the operation.
    \item If the list ends prematurely then the calculation so far is returned.
    \item If a \fn{Nothing} behaves like number \fn{N},
          then it \emph{does} contribute to the count, for \fn{fixed} opcodes.
    \item If a \fn{Nothing} is \fn{skip}ped for a \fn{fixed N} opcode, then it does \emph{not} contribute to the count.
    \item If the \fn{stop@} number is the same
          as the number to replace \fn{Nothing},
          then \fn{Nothing} is treated like its value,
          and does \emph{not} stop the calculation
  \end{itemize}
\end{itemize}

\begin{code}
specifyQ4 :: String -> [Int] -> IO String
specifyQ4 mname ell 
  = do let table = genQ4table ell
       return $ unlines (pre4 ++ table ++ post4)
  where
    pre4 = 
      [ ("-- Q4 ("++show ex2q4mark++" marks)")
      , "f4 :: [Maybe Int] -> (Int,[Maybe Int])"
      , "-- Operation Table (See Exercise2 description on BB)" ]
    post4 = 
      [ "f4 mis = undefined" ]
\end{code}

\newpage
\begin{code}
data ZOpSpec = OS Int String String String deriving (Eq, Ord)
instance Show ZOpSpec where
  show (OS opc oname orands nothing)
    = "--    |   "++show opc++"   |    "++oname++"    | "++orands++"  | "
      ++ nothing ++ "    |"

genQ4table ell
  = top : banner : stripe: (map show ops) ++ [stripe]
  where 
    top    = "--    ___________________________________________"
    banner = "--    | opcode | operation | operands | Nothing |"
    stripe = "--    -------------------------------------------"
    (opcodes,details) = splitAt 12 ell
    (addopcodes,mulopcodes) = splitAt 6 opcodes
    (adddetails,muldetails) = splitAt 8 details
    addOps = genOpRows "add" addopcodes adddetails
    mulOps = genOpRows "mul" mulopcodes muldetails
    ops = addOps++mulOps


genOpRows op 
  [opc1,opc2,opc3,opc4,opc5,opc6] 
  [fs1,fs2,fs3,fdef,ss1,ss2,ss3,sdef]
  = [ OS opc1 op ("fixed "++show fs1) "term"
    , OS opc2 op ("fixed "++show fs2) "skip"
    , OS opc3 op ("fixed "++show fs3) (show fdef ++ "   ")
    , OS opc4 op ("stop@ "++show ss1) "term"
    , OS opc5 op ("stop@ "++show ss2) "skip"
    , OS opc6 op ("stop@ "++show ss3) (show sdef ++ "   ") ]

suppliedQ4 :: [Int] -> String
suppliedQ4 ell = unlines 
  [ "add :: Int -> Int -> Int"
  , "add x y = (x+y) `mod` 65563"
  , ""
  , "mul :: Int -> Int -> Int"
  , "mul x y"
  , "  | p == 0    = 1"
  , "  | otherwise = p"
  , "  where p = (x*y) `mod` 65563"
  ]
\end{code}

\begin{code}
specifyT4 :: String -> [Int] -> IO (String, String)
specifyT4 mname ell 
  = return 
     ( supportT4 ell
     , 
       unlines 
        [ testGroupName theExerciseNumber 4
          ++ " = testGroup \"\\nQ4 ("++show ex2q4mark++" marks)\" ["
        , "    testCase \"Q4T1 nil [1 mark]\" "
        , "      ( f4 noMInts @?= truef4 noMInts )"
        , "  , testCase \"Q4T2 (0) [1 mark]\" "
        , "      ( f4 zero @?= truef4 zero )"
        , "  , testCase \"Q4T3 (N) [1 mark]\" "
        , "      ( f4 nowt @?= truef4 nowt )"
        , "  , testCase \"Q4T4 (999) [1 mark]\" "
        , "      ( f4 j999 @?= truef4 j999 )"
        , "  , testGroup \"Q4T5 well-behaved (12 marks)\""
        , "      $ map wbTC $ zip [1..] theOpcodes"
        , "  , testGroup \"Q4T6 bad-first (12 marks)\""
        , "      $ map fbTC $ zip [1..] theOpcodes"
        , "  , testGroup \"Q4T7 bad-middle (12 marks)\""
        , "      $ map mbTC $ zip [1..] theOpcodes"
        , "  , testGroup \"Q4T8 bad-last (12 marks)\""
        , "      $ map lbTC $ zip [1..] theOpcodes"
        , "  ]"
        ] 
      )

supportT4 :: [Int] -> String
supportT4 ell = unlines 
  [ "-- extra support for T4 contained in Test2Support"
  , "(table,truef4)"
  , "  = setup opcodes opconfig"
  , "  where"
  , "    opcodes  = " ++ show (take 12 ell)
  , "    opconfig = " ++ show (drop 12 ell)
  , "theOpcodes = getZOSpec table"
  , ""
  , "j999 = [Just 999]"
  , ""
  , "wbTC (osNo,(ZOSpec oc opname op start howmany errhndl))"
  , "  = testCase (\"Q4T5 well-behaved(\"++show oc++\") [1 mark]\")"
  , "      ( let wb = wellBehaved (theOpcodes!!(osNo-1)) ++ j999"
  , "        in (f4 wb @?= truef4 wb ) )"
  , ""
  , "fbTC (osNo,(ZOSpec oc opname op start howmany errhndl))"
  , "  = testCase (\"Q4T6 bad-first (\"++show oc++\") [1 mark]\")"
  , "      ( let bf = badFirst (theOpcodes!!(osNo-1)) ++ j999"
  , "        in (f4 bf @?= truef4 bf ) )"
  , ""
  , "mbTC (osNo,(ZOSpec oc opname op start howmany errhndl))"
  , "  = testCase (\"Q4T7 bad-middle (\"++show oc++\") [1 mark]\")"
  , "      ( let bm = badMiddle (theOpcodes!!(osNo-1)) ++ j999"
  , "        in (f4 bm @?= truef4 bm ) )"
  , ""
  , "lbTC (osNo,(ZOSpec oc opname op start howmany errhndl))"
  , "  = testCase (\"Q4T8 bad-last (\"++show oc++\") [1 mark]\")"
  , "      ( let bl = badLast (theOpcodes!!(osNo-1)) ++ j999"
  , "        in (f4 bl @?= truef4 bl ) )"
  ]
\end{code}

\newpage
\subsubsection{Ex 2 Q 5}

\ExTwoQV

Use \fn{f4} above in a function with signature \fn{[Maybe Int] -> [Int]}
that processes all operations in the list.


\begin{code}
specifyQ5 :: String -> IO String
specifyQ5 mname  
  = return $ unlines $
      [ ("-- Q5 ("++show ex2q5mark++" marks)")
      , "f5 :: [Maybe Int] -> [Int]"
      , "-- uses f4 to process all the opcodes in the maybe list,"
      , "-- by repeatedly applying it to the leftover part"
      , "f5 mis = undefined"
      ]
\end{code}

\begin{code}
specifyT5 :: String -> IO (String, String)
specifyT5 mname  
  = return 
     ( supportT5 
     , 
       unlines 
        [ testGroupName theExerciseNumber 5
          ++ " = testGroup \"\\nQ5 ("++show ex2q5mark++" marks)\" ["
        , "    testCase \"Q5T1 nil [1 mark]\" "
        , "      ( (f5 []) @?= (run truef4 []) )"
        , "  , testCase \"Q5T2 all [1 mark]\" "
        , "      ( let alloc = (concat $ map wellBehaved theOpcodes) ++ j999"
        , "        in (f5 alloc @?= run truef4 alloc) )"
        , "  ]"
        ] 
      )

supportT5 :: String
supportT5 = unlines 
  [ "-- T5 uses T4 support"
  ]
\end{code}
