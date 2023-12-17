\subsection{Parse \texttt{stack test} Output}
\begin{code}
module 
  ParseStackTest ( 
    Outcome(..), TestDetail, Grading(..)
  , getQNo, getQTNos
  , gradeMarks, gradeText
  , parseStackTest 
  ) 
where

import Data.List
import Data.Char
import System.FilePath

import Debug.Trace

dbg s x = trace (s ++ show x) x
pdbg s x = dbg (s ++ ":\n") x
\end{code}

\subsubsection{Analysis}

Warnings and compile-errors have full filepaths in log.
Key events are lines starting with \texttt{exN> }.

Key lines to note:
\begin{description}
%
\item[All OK]~
\begin{verbatim}
ex2> configure (lib + exe + test)
ex2> build (lib + exe + test)
exN> test (suite: Main)
TEST Exercise 01 (10 marks):
TEST Ex2:
Q2 (3 marks):
  Q1 T1 [10 marks]: [OK]
  Q2T1 nil [1 mark]: [OK]
  Q4T5 well-behaved(93) [1 mark]: [OK]    -- !!!!!
  Q4T6 bad-first (25) [1 mark]: [OK]      -- !!!!!
exN> Test suite Main passed
\end{verbatim} 
%
\item[Some OK]~
\begin{verbatim}
exN> configure (lib + exe + test)
exN> build (lib + exe + test)
exN> test (suite: Main)
---- begin examples
TEST Exercise 01 (10 marks):
--
  Q1 T1 [10 marks]: [Failed]
expected: 24205998
  but got: 13094887
--
    Q5T2 all [1 mark]: [Failed]
expected: [8,6,16,10,32,16,8,8,8,32,8,16,0]
 but got: [8,6,16,10,32,16,8,8,8,32,8,16]
--
Main: src/Ex2.hs:88:1-36: Non-exhaustive patterns in function mulUntilNothingOrN
---- end examples
ex2> Test suite Main failed
\end{verbatim}
%
\newpage
\item[Not Compiled]~
\begin{verbatim}
exN> configure (lib + exe + test)
exN> build (lib + exe + test)
-- followed by:

/Users/butrfeld/Desktop/FP-Exercises/unpack/uname/Test1/src/Ex1.hs:1:1: error:
Error: [S-7282]
       [S-7011]
       Process exited with code: ExitFailure 1
--
/Users/butrfeld/Desktop/GradeEX2/unpack/uname/Test2/src/Ex2.hs:90:9: error: parse error on input ‘answer’
   |
90 |         answer = operation ans ((add (isNothings ops) (val - length isNothings ops))) -- gets the operation type -> the operation is used on the accumulator ans & the sum of valid values + corrupted values
   |         ^^^^^^
--
Q1 (3 marks):
    Q1T1 (1..i-1)  [1 mark]: [Failed]
Main: Prelude.!!: index too large
--
/Users/butrfeld/Desktop/GradeEX2/unpack/uname/Test2/src/Ex2.hs:67:32: error: Variable not in scope: term :: Int
   |
67 |     23 -> applyOperation add 5 term rest
   |                                ^^^^
--
Q1 (3 marks):
    Q1T1 (1..i-1)  [1 mark]: [Failed]
Main: Prelude.head: empty list
\end{verbatim}
%
\end{description}
In addition to the TEST output we want to parse lines of the form:
\begin{verbatim}
exN> ... some stuff ...
/Users/.../uname/Test1/src/ExN.hs:L:P: error:
Error: [S-7282]
       [S-7011]
       Process exited with code: ExitFailure 1
\end{verbatim}
The \texttt{exN>} and \texttt{/Users/.../} 
are contingent on the particular exercise and the individual doing the grading,
so they should (ultimately) be placed in a configuration file.
The other lines should be fixed (barring any major change in \texttt{stack} output)

\subsubsection{Lexical Analysis}

\begin{code}
data LogLexeme
  =  Step [String] -- e.g., "exN> configure (lib + exe + test)"
  |  Install String -- e.g., "Installing executable exN in /Users/.../uname/.../bin"
  |  TestHdr [String] -- e.g., "TEST Exercise 01 (10 marks):" or "Q1 (3 marks):"
  |  TestOK [String]    -- "  Q1 T1 [10 marks]: [OK]" 4 words
                        -- "  Q4T6 bad-first (25) [1 mark]: [OK]" 5 words
  |  TestFail  [String] -- "  Q1 T1 [10 marks]: [Failed]" 5 words
                        -- "  Q4T6 bad-first (25) [1 mark]: [Failed]" 5 words
  |  TestERROR [String] -- e.g., "ERROR: Prelude.undefined"
  |  TestExp [String] -- .e.g., "expected: 24205998"
  |  TestGot [String] -- .e.g., "  but got: 13094887"
  |  DNC String [String]  
       -- .e.g., "/Users/.../uname/Test1/src/ExN.hs:L:P: error:  error-details"
  |  Junk
  deriving Show

logLexer :: String -> LogLexeme
logLexer line = loglex $ words line

-- a lot of this is hardwired for now 
-- it needs some config parameters in general
--  "ex>1"  should be "ex>"++show N
-- as should be gradedir below !
-- /Users/butrfeld/Desktop/FP-Exercises/unpack/uname/Test1/src/Ex1.hs
dropPaths n path = joinPath $ drop n $ splitPath path
relPath path = joinPath $ trimPath $ splitPath path
-- want 'src/ExN.hs'
trimPath []      = []
trimPath p@[_]   = p
trimPath p@[_,_] = p
trimPath (_:p)   = trimPath p


loglex :: [String] -> LogLexeme
loglex ["Installing",_,_,_,path]  =  Install path
loglex ws@(w1:wrest@(w2:wrest'))
  | isExPrompt w1  =  Step wrest
  | w1 == "TEST"   =  TestHdr wrest
  | head w1 == 'Q' && last wrest == "[OK]"      =  TestOK ws
  | head w1 == 'Q' && last wrest == "[Failed]"  =  TestFail ws
  | head w1 == 'Q'               =  TestHdr wrest
  | w1 == "ERROR:"               =  TestERROR wrest
  | w1 == "expected:"            =  TestExp wrest
  | w1 == "but" && w2 == "got:"  =  TestGot wrest' 
  | w2 == "error:"               =  DNC w1 wrest'
  where 
    isExPrompt ('e':'x':d:'>':[])  =  isDigit d
    isExPrompt _                   =  False

loglex _ = Junk


useful Junk  =  False
useful _     =  True
\end{code}

\subsubsection{Datatypes}

\begin{code}
data Outcome = OK | Fail String deriving (Eq, Show)
type TestDetail = ( String   -- qtno
                  , Int      -- raw mark
                  , Outcome   -- feedback
                  )
data Grading
  =  IllFormed String
  |  WrongUser String
  |  DidNotCompile String [String]
  |  TestResults [TestDetail]
  deriving Show

gradeMarks :: Grading -> Int
gradeMarks _                    =  0

illFormed = "XX "
wrongUser = "USER? "
dnc = "COMP-ERROR "
someTestsFail = "SOME-FAIL "
allTestsPass = "ALL-PASS "
gradeText :: Grading -> String
gradeText (IllFormed s)         =  illFormed++s
gradeText (WrongUser u)         =  wrongUser++u
gradeText (DidNotCompile r rs)  =  dnc++r++" "++unwords rs
gradeText (TestResults results)
  | any badResult results  
      =  someTestsFail 
         ++ concat ( intersperse " " 
                        (map dispOutcome $ filter badResult results))
  | otherwise                  =  allTestsPass

badResult :: TestDetail -> Bool
badResult (_,_,oc) =  oc /= OK

dispOutcome :: TestDetail -> String
dispOutcome (qtno,pts,OK) =  ""
dispOutcome (qtno,pts,Fail fbk) =  qtno++"("++show pts++")"++fbk
\end{code}


\begin{code}
computeGrade :: String -> [LogLexeme] -> Grading
computeGrade uname [] = IllFormed "No Log!"
computeGrade uname lls = scanConfig uname lls

-- seen nothing yet, expect 'configure'
scanConfig uname (Step ("configure":_):lls) = scanBuild uname lls 
scanConfig uname (ll:lls) = IllFormed "'configure' expected"

-- seen 'configure', expect 'build'
scanBuild uname (Step ("build":_):lls) = scanCRE uname lls 
scanBuild _ _  = IllFormed "'build' expected"

-- seen 'build', expect 'copy/register' or 'error'
scanCRE uname (Step ("copy/register":_):lls) = scanInstalled uname lls 
scanCRE uname (DNC s rs:_) = DidNotCompile (relPath s) rs
scanCRE _ _  = IllFormed "'copy/register' or 'error' expected"

-- seen 'copy/register', expect 'Installed'
scanInstalled uname ((Install _):lls) = scan_test uname lls 
scanInstalled _ _  = IllFormed "'Installed' expected"

-- seen 'Installed', expect 'test'
scan_test uname (Step ("test":_):lls) = scanTest uname lls 
scan_test _ lls  = IllFormed ("'test' expected"++show lls)  -- *****

-- seen 'test', expect 'TEST/Qn'
scanTest uname (TestHdr ss : lls) 
                             = scanTests uname (TestResults []) lls 
scanTest _ _  = IllFormed "'TEST' expected"

-- seen 'TEST', expect `Qn` hdr, tests or 'Test suite Main' outcome
scanTests uname g ((TestHdr _) : lls)  =  scanTests uname g lls 

-- can see OK test:
scanTests uname (TestResults msg) (TestOK [q,t,pt,m,ok] : lls)
  | newpts > 0  =  scanTests uname (TestResults (msg++[(q,newpts,OK)])) lls 
  where newpts = getPoints pt
scanTests uname (TestResults msg) (TestOK [q,t1,t2,pt,m,ok] : lls)
  | newpts > 0  =  scanTests uname (TestResults (msg++[(q,newpts,OK)])) lls 
  where newpts = getPoints pt

-- can see Failed test:
scanTests uname (TestResults msg) (TestFail [q,t,pt,m,fl] : lls)
  =  scanFailed uname q pt (TestResults msg) lls 
scanTests uname (TestResults msg) (TestFail [q,t1,t2,pt,m,fl] : lls)
  =  scanFailed uname q pt (TestResults msg) lls 

-- can see test outcome
scanTests uname g@(TestResults msg) 
               (Step x@[test,suite,name,outcome] : lls)
  | test == "Test" && suite == "suite" && name == "Main"
      = case outcome of
          "passed"  ->  g 
          "failed"  ->  g
          _         ->  IllFormed ("test outcome expected: "++show x)
scanTests _ _ lls = IllFormed ("tests or test outcome expected"++show lls)

--- seen test fail, expect 'expected:' or 'ERROR:'
scanFailed uname qno pt (TestResults msg) (TestExp ss : lls)
  = scanButGot uname qno pt ss (TestResults msg) lls
scanFailed uname q pt (TestResults msg) (TestERROR ss : lls)
  = scanTests uname (TestResults (msg++[(q,lost,Fail sscat)])) lls
  where lost = getPoints pt
        sscat   = spconcat ss
scanFailed _ _ _ _ _ = IllFormed "'expected:' or 'ERROR:' expected"

--- seen 'expected:', expect 'but got:'
scanButGot uname q pt exps (TestResults msg) (TestGot ss : lls)
  = scanTests uname (TestResults (msg++[(q,lost,Fail fbk)])) lls
  where lost = getPoints pt
        fbk = "!:"++spconcat exps++" ?:"++spconcat ss
scanButGot _ _ _ _ _ _ = IllFormed "'but got:' expected"


getQNo :: String -> Int
getQNo ('Q':d:rest)
  | isDigit d  =  read (d:takeWhile isDigit rest)
getQNo _ = 0

getQTNos :: String -> (Int,Int)
getQTNos ('Q':d:rest)
  | isDigit d = getQTNos' (read (d:qdigits)) tpart
  where 
    (qdigits,tpart) = span isDigit rest
getQTNos _ = (0,0)

getQTNos' qno ('T':d:rest)
  | isDigit d = (qno,read (d:takeWhile isDigit rest))
getQTNos' _ _ = (0,0)

getPoints :: String -> Int
getPoints ('[':ns@(_:_)) | all isDigit ns  =  read ns
getPoints _                                =  -1

spconcat = map uncomma . concat . intersperse " "
uncomma ',' =  ' '
uncomma c   =  c
\end{code}

% \begin{code}
% \end{code}

% \begin{code}
% \end{code}



\subsubsection{Parser}


\begin{code}
parseStackTest :: String -> [String] -> Grading
parseStackTest uname loglines
  =  let 
       lexemes = filter useful $ map logLexer loglines
     in computeGrade uname lexemes


-- debugging

getlog fn =
  do txt <- readFile fn 
     return $ lines txt

okfn = "templates/test-OK.log"
wrfn = "templates/test-WRONG.log"
erfn = "templates/test-ERROR.log"

parse = parseStackTest "memyselfi"
\end{code}

