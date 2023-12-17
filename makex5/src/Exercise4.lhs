\subsection{Exercise Four}

\begin{code}
module 
  Exercise4( 
    theExerciseNumber, exerciseFolder, testFolder
  , generateExercise4, generateTests4
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
theExerciseNumber = (4 :: Int)
exerciseFolder = "Exercise"++show theExerciseNumber
testFolder = "Test"++show theExerciseNumber
\end{code}

\newpage
\subsubsection{Exercise Planning}

\input{exercises/Exercise-4-Plan}

\begin{code}
generateExercise4 :: String -> Config -> IO ()
generateExercise4 folder config
  = do putStr $ showConfig config
       putStrLn "Generating EX4"
       let sg0 = mkStdGen $ read $ idno config
       sg <- genEx4 folder config sg0
       return ()
\end{code}

\begin{code}
ex4q1mark = 8
ex4q2mark = 6
ex4q3mark = 11
ex4marks = sum [ex4q1mark,ex4q2mark,ex4q3mark]

genEx4 :: String -> Config -> StdGen -> IO StdGen
genEx4 folder config sg 
  = do ([ex4rn1,ex4rn2,ex4rn3],sg) <- genExercise4RNs sg

       putStrLn ("Ex4 Q1 ("++show ex4q1mark++") rn = "++show ex4rn1)
       q1 <- specifyQ1 ex4rn1
       putStrLn ("Ex4 Q2 ("++show ex4q2mark++") rn = "++show ex4rn2)
       q2 <- specifyQ2 ex4rn2
       putStrLn ("Ex4 Q3 ("++show ex4q3mark++") rn = "++show ex4rn3)
       q3 <- specifyQ3 ex4rn3
       putStrLn ("Ex4 - Total Marks: "++show ex4marks)
      
       let supplied = concat $ intersperse "\n" 
                        [ suppliedQ1 ex4rn1
                        , suppliedQ2 ex4rn2
                        , suppliedQ3 ex4rn3
                        ]
       let preamble = exercisePreamble supplied theExerciseNumber 0
       let qs = concat $ intersperse "\n" [q1,q2,q3]
       writeFile fname (preamble 
                        ++ qs 
                        ++ exercisePostamble theExerciseNumber 0)
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

\newpage
\subsubsection{Test Generation}

\begin{code}
generateTests4 :: Config -> IO ()
generateTests4 config
  = do putStr $ showConfig config
       putStrLn "Generating T4"
       let sg0 = mkStdGen $ read $ idno config
       sg <- genTst4 config sg0
       return ()
\end{code}


\begin{code}
genTst4 :: Config -> StdGen -> IO StdGen
genTst4 config sg 
  = do ([ex4rn1,ex4rn2,ex4rn3],sg) <- genExercise4RNs sg

       putStrLn ("Ex4 T1 rn1 = "++show ex4rn1)
       (suppt1,t1) <- specifyT1 mname ex4rn1 
       putStrLn ("Ex4 T2 rn2 = "++show ex4rn2)
       (suppt2,t2) <- specifyT2 mname ex4rn2
       putStrLn ("Ex4 T3 rn3 = "++show ex4rn3)
       (suppt3,t3) <- specifyT3 mname ex4rn3

       let support = concat $ intersperse "\n" 
                       [suppt1,suppt2,suppt3]
       let tests = concat $ intersperse "\n" [t1,t2,t3]
       writeFile fname 
         ( specPreamble 
              "import Test4Support" theExerciseNumber 
           ++ specBody theExerciseNumber 4
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
\subsubsection{Ex 4 Q 1}

\ExFourQI

\begin{code}
suppliedQ1 :: [Int] -> String
suppliedQ1 rn@[ ex,val,var,dvd, binN,binNV,uniN,uniNV, binB,binBV,uniB,uniBV ]
  = unlines 
      [ "--required for Q1"
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
      , "find :: MonadFail m => String -> Dict -> m Float"
      , "find s [] = fail (s++\" not found\")"
      , "find s ((t,f):d)"
      , "  | s==t       =  return f"
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
specifyQ1 rn1@(ex:_) 
  = return $ unlines $
      [ ("-- Q1 ("++show ex4q1mark++" marks)")
      , hc "implement the following function (which always returns a value):"
      , "mdeval :: MonadFail m => Dict -> "++expr++" -> m Float"
      , "mdeval _ _ = error \"Ex4Q1: mdeval not yet defined\""
      ]
  where
    (expr,_) = alookup aliases "Expr" ex
\end{code}

\begin{code}
specifyT1 :: String -> [Int] -> IO (String, String)
specifyT1 mname rn1 
  = return 
     ( supportT1 rn1
     , 
       unlines 
        [ testGroupName theExerciseNumber 1
          ++ " = testGroup \"\\nQ1 ("++show ex4q1mark++" marks)\" ["
        , "  ]"
        ] 
      )
      where istr = show rn1

supportT1 :: [Int] -> String
supportT1 rn1 = unlines 
  [ hc "T1 support contained in Test4Support"
  ]
\end{code}


\newpage
\subsubsection{Ex 4 Q 2}

\ExFourQII

\begin{code}
suppliedQ2 :: [Int] -> String
suppliedQ2 [foldlorr,tupleorder,tuple,mtype,ntype]
  = unlines 
      [ hc "required for Q2"
      , "x `incfst` _  =  x + 1"
      , "_ `incsnd` y  =  1 + y"
      , "type Thing = " ++ genTuple tuple (genType mtype) (genType ntype)
      ]

genType :: Int -> String
genType 0 = "Bool"
genType 1 = "Int"
genType 2 = "Float"
genTuple :: Int -> String -> String -> String
genTuple 0 mt nt = "("++mt++",["++nt++"])"
genTuple 1 mt nt = "(["++mt++"],"++nt++")"
genTuple 2 mt nt = "(["++mt++"],["++nt++"])"
\end{code}


\begin{code}
specifyQ2 :: [Int] -> IO String
specifyQ2 rn2@[foldlorr,tupleorder,tuple,mtype,ntype] 
  = return $ unlines $
      [ ("-- Q2 ("++show ex4q1mark++" marks)")
      , hc "Consider the following four recursive pattern definitions:" 
      , "len :: Int -> [Int] -> Int" ]
      ++ genRec foldlorr "len" "z" "x" "xs" (genInc foldlorr)
      ++ [ "sumup :: Int -> [Int] -> Int"]
      ++ genRec foldlorr "sumup" "sbase" "n" "ns" "+"
      ++ [ "prod :: Int -> [Int] -> Int"]
      ++ genRec foldlorr "prod" "mbase" "n" "ns" "*" 
      ++ [ "cat :: [Thing] -> [[Thing]] -> [Thing]"]
      ++ genRec foldlorr "cat" "pfx" "xs" "xss" "++" 
      ++
      [ ""
      , hc "They all have the same abstract pattern,"
      , hc "as captured by the following Higher Order Function (HOF):"] 
      ++ genHOF foldlorr ++ 
      [ ""
      , hc ("We can gather the `z` and `opr` arguments into a tuple: "
             ++ order)
      , hc ("which allows us to construct a call to "++fold++" as:")
      , "dofold "++order++" = "++fold++" z op"
      , ""
      , hc "Your task is to complete the tuples below,"
      , hc "so that `dofold` can be used to implement the fns. above."
      , ""
      , hc "dofold lenTuple = len"
      , "lenTuple :: " ++ genOrder tupleorder "Int" "Int -> Int -> Int"
      , "lenTuple = (undefined,undefined)" 
      , ""
      , hc "dofold sumupTuple = sumup"
      , "sumupTuple :: " ++ genOrder tupleorder "Int" "Int -> Int -> Int"
      , "sumupTuple = (undefined,undefined)" 
      , ""
      , hc "dofold prodTuple = prod"
      , "prodTuple :: " ++ genOrder tupleorder "Int" "Int -> Int -> Int"
      , "prodTuple = (undefined,undefined)" 
      , ""
      , hc "dofold catTuple = cat"
      , "catTuple :: " ++ genOrder tupleorder "[Thing]" "[Thing] -> [Thing] -> [Thing]"
      , "catTuple = (undefined,undefined)" 
      ]
  where 
    fold = genFold foldlorr
    order = genOrder tupleorder "z" "op"


genRec :: Int -> String -> String -> String -> String -> String -> [String]
genRec 0 fn z x xs opr 
 = [ fn ++ " " ++ z ++ " []     = " ++ z
   , fn ++ " " ++ z ++ " ("++x++":"++xs++") = " 
        ++ fn ++ " ("++z++" "++opr++" "++x++") "++xs
   ]
genRec 1 fn z x xs opr 
 = [ fn ++ " " ++ z ++ " []     = " ++ z
   , fn ++ " " ++ z ++ " ("++x++":"++xs++") = " 
        ++ x ++ " " ++ opr ++ " (" ++ fn ++ " "++z++" "++xs++")"
   ]

genInc 0 = "`incfst`"
genInc 1 = "`incsnd`"

genFold 0 = "foldL" ; genFold 1 = "foldR"

genHOF :: Int -> [String]
genHOF 0 = [ "foldL z _ [] = z"
           , "foldL z op (x:xs) = foldL (z `op` x) op xs"
           ]
genHOF 1 = [ "foldR z _ [] = z"
           , "foldR z op (x:xs) = x `op` foldR z op xs"
           ]

genOrder :: Int -> String -> String -> String
genOrder 0 z op = "("++z++","++op++")"
genOrder 1 z op = "("++op++","++z++")"
\end{code}

\begin{code}
specifyT2 :: String -> [Int] -> IO (String, String)
specifyT2 mname rn2@[foldlorr,tupleorder,tuple,mtype,ntype] 
  = return 
     ( supportT2 [foldlorr,tupleorder,tuple,mtype,ntype]
     , 
       unlines 
        [ testGroupName theExerciseNumber 2
          ++ " = testGroup \"\\nQ2 ("++show ex4q2mark++" marks)\" ["
        , "  ]"
        ] 
      )
      where istr = show rn2

supportT2 :: [Int] -> String
supportT2 [foldlorr,tupleorder,tuple,mtype,ntype] = unlines 
  [ 
  ]
\end{code}


\newpage
\subsubsection{Ex 4 Q 3}

\ExFourQIII

We have a list of random numbers of the form:
 $$\langle n, t_1, \dots t_n, k_1, \dots, k_n \rangle$$
We want to convert that to the following:
 $$ \langle (t_1,k_1) , \dots, (t_n,k_n)\rangle$$
Here $t_i \in \{0,\dots,3\}$ denotes $\{(+k),(\times k),(-k),(k-)\}$,
while $k_i$ denotes, well, $k$.
\begin{code}
parseOps (n:rest) -- we assume length rest == 2 * n
  = zip ts ks
  where (ts,ks) = splitAt n rest
mkSection :: (Int,Int) -> String
mkSection (0,k) = "(+"++show k++")"
mkSection (1,k) = "(*"++show k++")"
mkSection (2,k) = "(sub "++show k++")"
mkSection (3,k) = "("++show k++"-)"
mkSections :: [(Int,Int)] -> String
mkSections tks = concat $ intersperse "," $ map mkSection tks
\end{code}


\begin{code}
suppliedQ3 :: [Int] -> String
suppliedQ3 rn
  = unlines 
      [ hc "required for all Qs:"
      ]
\end{code}


\begin{code}
specifyQ3 :: [Int] -> IO String
specifyQ3 rn3 
  = return $ unlines $
      [ ("-- Q3 ("++show ex4q3mark++" marks)")
      , "sub = subtract -- shorter!"
      , "ops = ["++mkSections (parseOps rn3)++"]"
      , ""
      , hc "(!) This question requires modifying Main.hs"
      , hc "See, and/or compile and run Main.hs for further details"
      ]
\end{code}

\begin{code}
specifyT3 :: String -> [Int] -> IO (String, String)
specifyT3 mname rn3 
  = return 
     ( supportT3 rn3
     , 
       unlines 
        [ testGroupName theExerciseNumber 3
          ++ " = testGroup \"\\nQ3 ("++show ex4q3mark++" marks)\" ["
        , "  ]"
        , hc $ show rn3
        ] 
      )

supportT3 :: [Int] -> String
supportT3 rn3 = unlines 
  [ "-- T3 support contained in Test3Support"
  ]
\end{code}


