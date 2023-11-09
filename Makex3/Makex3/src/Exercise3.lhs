\subsection{Exercise Three}

\begin{code}
module 
  Exercise3( 
    theExerciseNumber, exerciseFolder, testFolder
  , generateExercise3, generateTests3 
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
import Data.Map(Map)
import qualified Data.Map as M
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
theExerciseNumber = (3 :: Int)
exerciseFolder = "Exercise"++show theExerciseNumber
testFolder = "Test"++show theExerciseNumber
\end{code}

\newpage
\subsubsection{Exercise Planning}

\input{exercises/Exercise-3-Plan}

\begin{code}
generateExercise3 :: String -> Config -> IO ()
generateExercise3 folder config
  = do putStr $ showConfig config
       putStrLn "Generating EX3"
       let sg0 = mkStdGen $ read $ idno config
       sg <- genEx3 folder config sg0
       return ()
\end{code}

\begin{code}
ex3q1mark = 8
ex3q2mark = 8
ex3q3mark = 4
ex3marks = sum [ex3q1mark,ex3q3mark,ex3q3mark]

genEx3 :: String -> Config -> StdGen -> IO StdGen
genEx3 folder config sg 
  = do ([ex3rn],sg) <- genExercise3RNs sg

       putStrLn ("Ex3 Q1 ("++show ex3q1mark++") rn = "++show ex3rn)
       q1 <- specifyQ1 ex3rn
       putStrLn ("Ex3 Q2 ("++show ex3q2mark++")")
       q2 <- specifyQ2 ex3rn
       putStrLn ("Ex3 Q3 ("++show ex3q3mark++")")
       q3 <- specifyQ3 ex3rn
       putStrLn ("Ex3 - Total Marks: "++show ex3marks)
      
       let supplied = suppliedQ1 ex3rn
       let preamble = exercisePreamble supplied theExerciseNumber
       let qs = concat $ intersperse "\n" [q1,q2,q3]
       writeFile fname (preamble ++ qs ++ exercisePostamble)
       return sg
  where
    nn = show $ exno config
    mname = "Ex"++nn
    fname = folder </> "src" </> mname <.> "hs"
\end{code}

\newpage
\subsubsection{Exercise Generation}

The idea:
\begin{code}
data Expr  -- aliases: CExpr EExpr GExpr Xprsn
  = Val Float  -- aliases: Value Number Literal
 | Var String   -- aliases: Vbl Variable VName VarNm
 | Dvd Expr Expr  -- aliases: Div Divide DvdBy Ratio
 | Not Expr  -- aliases: none!
 -- a subset of the following is included
 | Add Expr Expr  -- aliases: Adder Addition Plus Sum
 | Sub Expr Expr  -- aliases: Diff Subtract Minus 
 | Mul Expr Expr  -- aliases: Times Product MulBy Multiply
 | Abs Expr       -- aliases: Absolute AbsV Magn Magnitude
 | Neg Expr       -- aliases: Negate AddInv
 -- the following use the C convention for true/false
 | Eq Expr Expr   -- aliases: Equal Eql Same
 | Zero Expr      -- aliases: IsZero IsNull IsNil
 | Less Expr Expr -- aliases: LessThan IsLess Smaller
 | Greater Expr Expr -- aliases: GrtThan IsGreater Bigger
 -- perhaps more?  Or (a zero-avoiding add)  And (multiply) etc...
\end{code}


The execution:
\begin{code}
type Alias = Map String (String,[String])
aliases :: Alias
aliases 
  = M.fromList 
      [ ("Expr",("the expression datatype"
                ,["CExpr","EExpr","GExpr","Xprsn"]))
      , ("Val",("floating-point value"
               ,["Value","Number","Literal"]))
      , ("Var",("variable/identifier name"
               ,["Vbl","Variable","VName","VarNm"]))
      , ("Dvd",("divide first by second"
               ,["Div","Divide","DvdBy","Ratio"]))
      , ("Not",("logical not",["Not"]))
      , ("Add",("adds both together"
               ,["Adder","Addition","Plus","Sum"]))
      , ("Sub",("subtracts second from first"
               ,["Diff","Subtract","Minus","TakeAway"]))
      , ("Mul",("multiplies both"
               ,["Times","Product","MulBy","Multiply"]))
      , ("Abs",("absolute value"
               ,["Absolute","AbsVal","Magnitude"]))
      , ("Neg",("numerical negation (-x)"
               ,["Negate","AddInv","Negative"]))
      , ("Eq",("True if both are the same"
              ,["Equal","Eql","Same"]))
      , ("Neq",("True if both are different"
               ,["NotEqual","NEql","Dfrnt"]))
      , ("Zero",("True if numeric value is zero"
                ,["IsZero","IsNull","IsNil"]))
      , ("NonZero",("True if numeric value is non-zero"
                   ,["NotZero","NotNull","NotNil"]))
      , ("Less",("True if first is less than second"
                ,["LessThan","IsLess","Smaller"]))
      , ("Greater",("True if first is greater than second "
                   ,["GrtThan","IsGreater","Bigger"]))
      ]
\end{code}
Note that each non-mandatory group below has constructors
with the same number of aliases
(important for uniform randomisation!).
We generate all the mandatory constructors,
plus one each of the four unary/binary variants.
\begin{code}
mandatory = ["Val","Var","Dvd","Not"]
unaryNum = ["Abs","Neg"]
binaryNum = ["Add","Sub","Mul"]
unaryBool = ["Zero","NonZero"]
binaryBool = ["Eq","Neq","Less","Greater"]
\end{code}

We have a lookup by name and number:
\begin{code}
alookup :: Alias -> String -> Int -> (String,String)
alookup alias name variant
  = case M.lookup name alias of
      Nothing -> error ("alias: '"++name++"' not found")
      Just (descr,variants) -> (variants!!variant,descr)      
\end{code}
\begin{code}
\end{code}
\begin{code}
\end{code}
\begin{code}
\end{code}
\begin{code}
\end{code}
\begin{code}
\end{code}

\newpage
\subsubsection{Test Generation}

\begin{code}
generateTests3 :: Config -> IO ()
generateTests3 config
  = do putStr $ showConfig config
       putStrLn "Generating T3"
       let sg0 = mkStdGen $ read $ idno config
       sg <- genTst3 config sg0
       return ()
\end{code}


\begin{code}
genTst3 :: Config -> StdGen -> IO StdGen
genTst3 config sg 
  = do ([ex3rn],sg) <- genExercise3RNs sg

       putStrLn ("Ex3 T1 rn = "++show ex3rn)
       (suppt1,t1) <- specifyT1 mname 1 -- x
       putStrLn ("Ex3 T2 rn = "++show ex3rn)
       (suppt2,t2) <- specifyT2 mname 2 -- x
       putStrLn ("Ex3 T3 rn = "++show ex3rn)
       (suppt3,t3) <- specifyT3 mname 3 -- x

       let support = concat $ intersperse "\n" 
                       [suppt1,suppt2,suppt3]
       let tests = concat $ intersperse "\n" [t1,t2,t3]
       writeFile fname 
         ( specPreamble 
              "import Test3Support" theExerciseNumber 
           ++ specBody theExerciseNumber 3
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
\subsubsection{Ex 3 Q 1}

\ExThreeQI

\begin{code}
suppliedQ1 :: [Int] -> String
suppliedQ1 rn@[ ex,val,var,dvd, binN,binNV,uniN,uniNV, binB,binBV,uniB,uniBV ]
  = unlines 
      [ "--required for all Qs:"
      , "data " ++ expr ++ cmt excmt
      , "  = " ++ vl ++ " Float" ++ cmt vlcmt
      , "  | " ++ vr ++ " String" ++ cmt vrcmt
      , "  | " ++ dv ++ exprp ++ exprp ++ cmt dvcmt
      , "  | " ++ binnstr ++ exprp ++ exprp ++ cmt binncmt
      , "  | " ++ uninstr ++ exprp ++ cmt unincmt
      , "  -- the following are boolean expressions (using numbers)"
      , "  -- the number 0.0 represents False, all others represent True."
      , "  | " ++ nt ++ exprp ++ cmt ntcmt
      , "  | " ++ binbstr ++ exprp ++ exprp ++ cmt binbcmt
      , "  | " ++ unibstr ++ exprp ++ cmt unibcmt
      , "  deriving (Eq,Ord,Show)"
      , ""
      , "type Dict = [(String,Float)]"
      , "insert :: String -> Float -> Dict -> Dict"
      , "insert s f d = (s,f):d"
      , "find :: String -> Dict -> Maybe Float"
      , "find s [] = Nothing"
      , "find s ((t,f):d)"
      , "  | s==t       =  Just f"
      , "  | otherwise  =  find s d"
      ]
  where
    aget = alookup aliases
    cmt = (" -- "++)
    (expr,excmt) = aget "Expr" ex
    exprp = ' ' : expr
    (vl,vlcmt) = aget "Val" val
    (vr,vrcmt) = aget "Var" var
    (dv,dvcmt) = aget "Dvd" dvd
    (nt,ntcmt) = aget "Not" 0
    binn = binaryNum!!binN
    (binnstr,binncmt) = aget binn binNV
    binb = binaryBool!!binB
    (binbstr,binbcmt) = aget binb binBV
    unin = unaryNum!!uniN
    (uninstr,unincmt) = aget unin uniNV
    unib = unaryBool!!uniB
    (unibstr,unibcmt) = aget unib uniBV
\end{code}


\begin{code}
specifyQ1 :: [Int] -> IO String
specifyQ1 (ex:_) 
  = return $ unlines $
      [ ("-- Q1 ("++show ex3q1mark++" marks)")
      , "-- implement the following function (which may have runtime errors):"
      , "eval :: Dict -> "++expr++" -> Float"
      , "eval _ _ = error \"Ex3Q1: eval not yet defined\""
      ]
  where
    (expr,_) = alookup aliases "Expr" ex
\end{code}

\begin{code}
specifyT1 :: String -> Int -> IO (String, String)
specifyT1 mname i 
  = return 
     ( supportT1 i
     , 
       unlines 
        [ testGroupName theExerciseNumber 1
          ++ " = testGroup \"\\nQ1 ("++show ex3q1mark++" marks)\" ["
        , "  ]"
        ] 
      )
      where istr = show i

supportT1 :: Int -> String
supportT1 i = unlines 
  [ "-- T1 support contained in Test3Support"
  ]
\end{code}


\newpage
\subsubsection{Ex 3 Q 2}

\ExThreeQII

\begin{code}
specifyQ2 :: [Int] -> IO String
specifyQ2 (ex:_)
  = return $ unlines $
      [ ("-- Q2 ("++show ex3q2mark++" marks)")
      , "-- implement the following function (which always returns a value):"
      , "meval :: Dict -> "++expr++" -> Maybe Float"
      , "meval _ _ = error \"Ex3Q2: meval not yet defined\""
      ]
  where
    (expr,_) = alookup aliases "Expr" ex
\end{code}

\begin{code}
specifyT2 :: String -> Int -> IO (String, String)
specifyT2 mname j 
  = return 
     ( supportT2 j
     , 
       unlines 
        [ testGroupName theExerciseNumber 2
          ++ " = testGroup \"\\nQ3 ("++show ex3q3mark++" marks)\" ["
        , "  ]"
        ] 
      ) where jstr = show j

supportT2 :: Int -> String
supportT2 j = unlines 
  [ "-- T2 uses T1 support"
  ]
\end{code}

\subsubsection{Ex 3 Q 3}

\ExThreeQIII

\begin{code}
specifyQ3 :: [Int] -> IO String
specifyQ3 (ex:_)
  = return $ unlines $
      [ ("-- Q3 ("++show ex3q3mark++" marks)")
      , "-- Laws of Arithmetic for this question:"
      , "--    x + 0 = x"
      , "--    0 + x = x"
      , "--    x - 0 = x"
      , "--    x - x = 0"
      , "--    x * 0 = 0"
      , "--    1 * x = x"
      , "-- The following function should implement the two laws applicable"
      , "-- for *your* "++expr++" datatype."
      , "simp :: "++expr++" -> " ++ expr
      , "simp _ = error \"Ex3Q3: simp not yet defined\""
      ]
  where
    (expr,_) = alookup aliases "Expr" ex
\end{code}

\begin{code}
specifyT3 :: String -> Int -> IO (String, String)
specifyT3 mname k 
  = return 
     ( supportT3 k
     , 
       unlines 
        [ testGroupName theExerciseNumber 3
          ++ " = testGroup \"\\nQ3 ("++show ex3q3mark++" marks)\" ["
        , "  ]"
        ] 
      ) where kstr = show k

supportT3 :: Int -> String
supportT3 k = unlines 
  [ "-- T3 uses T1 support"
  ]
\end{code}


