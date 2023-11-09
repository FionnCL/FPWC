\subsection{Parse \texttt{stack test} Output}
\begin{code}
module 
  ParseStackTest ( 
    Grading(..)
  , gradeMarks, gradeText
  , parseStackTest 
  ) 
where

import Data.List
import Data.Char
import System.FilePath

import Debug.Trace

dbg s x = trace (s ++ show x) x
\end{code}

\subsubsection{Analysis}

Warnings and compile-errors have full filepaths in log.
Key events are lines starting with \texttt{ex1> }.

Key lines to note:
\begin{description}
\item[All OK]~
\begin{verbatim}
ex1> configure (lib + exe + test)
ex1> build (lib + exe + test)
ex1> copy/register
ex1> test (suite: Main)
TEST Exercise 01 (10 marks):
  Q1 T1 [10 marks]: [OK]
ex1> Test suite Main passed
Completed 2 action(s).
\end{verbatim} 
\item[Some OK]~
\begin{verbatim}
ex1> configure (lib + exe + test)
ex1> build (lib + exe + test)
ex1> copy/register
ex1> test (suite: Main)
TEST Exercise 01 (10 marks):
  Q1 T1 [10 marks]: [Failed]
expected: 24205998
  but got: 13094887
ex1> Test suite Main failed
Error: [S-7282]
\end{verbatim}
\item[Not Compiled]~
\begin{verbatim}
ex1> configure (lib + exe + test)
ex1> build (lib + exe + test)
/Users/butrfeld/Desktop/FP-Exercises/unpack/uname/Test1/src/Ex1.hs:1:1: error:
Error: [S-7282]
       [S-7011]
       Process exited with code: ExitFailure 1
\end{verbatim}
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
  =  Step [String] -- e.g., "ex1> configure (lib + exe + test)"
  |  Install String -- e.g., "Installing executable ex1 in /Users/.../uname/.../bin"
  |  TestHdr [String] -- e.g., "TEST Exercise 01 (10 marks):"
  |  TestOK [String] -- .e.g., "  Q1 T1 [10 marks]: [OK]"
  |  TestFail  [String] -- .e.g., "  Q1 T1 [10 marks]: [Failed]"
  |  TestERROR [String] -- e.g., "ERROR: Prelude.undefined"
  |  TestExp [String] -- .e.g., "expected: 24205998"
  |  TestGot [String] -- .e.g., "  but got: 13094887"
  |  DNC String -- .e.g., "/Users/.../uname/Test1/src/ExN.hs:L:P: error:"
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
  | w1 == "ex1>"  =  Step wrest
  | w1 == "TEST"  =  TestHdr wrest
  | head w1 == 'Q' && last wrest == "[OK]"  =  TestOK ws
  | head w1 == 'Q' && last wrest == "[Failed]"  =  TestFail ws
  | w1 == "ERROR:"  =  TestERROR wrest
  | w1 == "expected:"  =  TestExp wrest
  | w1 == "but" && w2 == "got:"  = TestGot wrest' 
  | w2 == "error:" && null wrest' = DNC w1
loglex _ = Junk


useful Junk  =  False
useful _     =  True
\end{code}

\subsubsection{Datatypes}

\begin{code}
data Grading
  =  IllFormed String
  |  WrongUser String
  |  DidNotCompile String
  |  SomeTestsFail Int String
  |  AllTestsPass Int
  deriving Show

gradeMarks :: Grading -> Int
gradeMarks (AllTestsPass m)     =  m
gradeMarks (SomeTestsFail m _)  =  m
gradeMarks _                    =  0

illFormed = "XX "
wrongUser = "USER? "
dnc = "COMP-ERROR "
someTestsFail = "SOME-FAIL "
allTestsPass = "ALL-PASS "
gradeText :: Grading -> String
gradeText (IllFormed s)        =  illFormed++s
gradeText (WrongUser u)        =  wrongUser++u
gradeText (DidNotCompile r)    =  dnc++r
gradeText (SomeTestsFail i m)  =  someTestsFail ++ show i ++ " " ++ m
gradeText (AllTestsPass i)     =  allTestsPass ++ show i
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
scanCRE uname (DNC s:_) = DidNotCompile $ relPath s 
scanCRE _ _  = IllFormed "'copy/register' or 'error' expected"

-- seen 'copy/register', expect 'Installed'
scanInstalled uname ((Install _):lls) = scan_test uname lls 
scanInstalled _ _  = IllFormed "'Installed' expected"

-- seen 'Installed', expect 'test'
scan_test uname (Step ("test":_):lls) = scanTest uname lls 
scan_test _ lls  = IllFormed ("'test' expected"++show lls)  -- *****

-- seen 'test', expect 'TEST'
scanTest uname (TestHdr ss : lls) = scanTests uname (SomeTestsFail 0 "TEST") lls 
scanTest _ _  = IllFormed "'TEST' expected"

-- seen 'TEST', expect tests or 'Test suite Main' outcome
-- can see OK test:
scanTests uname (SomeTestsFail pts msg) (TestOK [q,t,pt,m,ok] : lls)
  | newpts > 0  =  scanTests uname (SomeTestsFail (pts+newpts) msg) lls 
  where newpts = getPoints pt

-- can see Failed test:
scanTests uname (SomeTestsFail pts msg) (TestFail [q,t,pt,m,ok] : lls)
  =  scanFailed uname (SomeTestsFail pts msg) lls 

-- can see test outcome
scanTests uname g@(SomeTestsFail pts msg) 
               (Step x@[test,suite,name,outcome] : lls)
  | test == "Test" && suite == "suite" && name == "Main"
      = case outcome of
          "passed"  ->  AllTestsPass pts
          "failed"  ->  g
          _         ->  IllFormed ("test outcome expected: "++show x)
scanTests _ _ lls = IllFormed ("tests or test outcome expected"++show lls)

--- seen test fail, expect 'expected:' or 'ERROR:'
scanFailed uname (SomeTestsFail pts msg) (TestExp ss : lls)
  = scanButGot uname (SomeTestsFail pts (msg++" ?:"++spconcat ss)) lls
scanFailed uname (SomeTestsFail pts msg) (TestERROR ss : lls)
  = scanTests uname (SomeTestsFail pts (msg++" "++spconcat ss)) lls
scanFailed _ _ _ = IllFormed "'expected:' or 'ERROR:' expected"

--- seen 'expected:', expect 'but got:'
scanButGot uname (SomeTestsFail pts msg) (TestGot ss : lls)
  = scanTests uname (SomeTestsFail pts (msg++" !:"++spconcat ss)) lls
scanButGot _ _ _ = IllFormed "'but got:' expected"


getPoints :: String -> Int
getPoints ('[':ns@(_:_)) | all isDigit ns  =  read ns
getPoints _                                =  -1

spconcat = concat . intersperse " "
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

