\subsection{Exercise Five}

\begin{code}
module 
  Exercise5( 
    theExerciseNumber, exerciseFolder, testFolder
  , generateExercise5, generateTests5
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
theExerciseNumber = (5 :: Int)
exerciseFolder = "Exercise"++show theExerciseNumber
testFolder = "Test"++show theExerciseNumber
\end{code}

\newpage
\subsubsection{Exercise Planning}

\input{exercises/Exercise-5-Plan}

\newpage
\begin{code}
generateExercise5 :: String -> Config -> IO ()
generateExercise5 folder config
  = do putStr $ showConfig config
       putStrLn "Generating EX5"
       let sg0 = mkStdGen $ read $ idno config
       sg <- genEx5 folder config sg0
       return ()
\end{code}

\begin{code}
ex5q1mark = 5
ex5q2mark = 5
ex5q3mark = 7
ex5q4mark = 8
ex5marks = sum [ex5q1mark,ex5q2mark,ex5q3mark,ex5q4mark]

genEx5 :: String -> Config -> StdGen -> IO StdGen
genEx5 folder config sg 
  = do ([ex5rn1,ex5rn2,ex5rn3,ex5rn4],sg) <- genExercise5RNs sg

       putStrLn ("Ex5 Q1 ("++show ex5q1mark++") rn = "++show ex5rn1)
       q1 <- specifyQ1 ex5rn1
       writeTheorem folder 1 q1
       putStrLn ("Ex5 Q2 ("++show ex5q2mark++") rn = "++show ex5rn2)
       q2 <- specifyQ2 ex5rn2
       writeTheorem folder 2 q2
       putStrLn ("Ex5 Q3 ("++show ex5q3mark++") rn = "++show ex5rn3)
       q3 <- specifyQ3 ex5rn3
       writeTheorem folder 3 q3
       putStrLn ("Ex5 Q4 ("++show ex5q4mark++") rn = "++show ex5rn4)
       q4 <- specifyQ4 ex5rn4
       writeTheorem folder 4 q4
       putStrLn ("Ex5 - Total Marks: "++show ex5marks)

       let supplied = intersperse "\n" 
                        [ suppliedQ1 ex5rn1
                        , suppliedQ2 ex5rn2
                        , suppliedQ3 ex5rn3
                        , suppliedQ4 ex5rn4
                        ]
       let suppPreamble  = [ "module Ex5 where" ]
       let suppBody = unlines (suppPreamble ++ supplied)
       writeFile (folder </> "Ex5" <.> "hs") suppBody

       return sg

writeTheorem folder qno qbody
  = writeFile (folder </> ("Ex5Q"++show qno) <.> "thr") qbody
\end{code}

\newpage
\subsubsection{Exercise Generation}

\paragraph{Question 1}~

Lets have a simple abstract syntax for Ex5 Q1 expressions:
\begin{code}
data HE = N Int     -- literal number
        | V String  -- variable
        | E         -- []
        | C HE HE   -- (_:_)
        | B String HE HE  -- (_ op _)
        | A String [HE]   --  fn _ _ ... _
        deriving (Eq,Show)
zero = N 0
one  = N 1
two  = N 2
n42  = N 42
nil  = E
[vx,vy,vz,vxs,vys,vzs] = map V ["x","y","z","xs","ys","zs"]
cons = C
[add,mul,sub,cat] = map B ["+","*","-","++"]
rev xs = A "rev" [xs]
len xs = A "len" [xs]
smm xs = A "sum" [xs] -- 'sum' is in Prelude
prd xs = A "prd" [xs]
rpl n x = A "rpl" [n,x]
\end{code}

We now implement ``expansive'' laws:
\begin{code}
alu,aru,mlu,mru,sru,rv2 :: HE -> HE
alu x = add zero x
aru x = add x zero
mlu x = mul one x
mru x = mul x one
sru x = sub x zero
rv2 xs = rev (rev xs)
lrp :: HE -> HE -> HE
lrp x n  = len (rpl n x)  -- we need to supply x, note re-ordering

crp :: HE -> HE -> HE -> HE
crp m n x  =  (rpl m x) `cat` (rpl n x) -- we need to have a specific split
\end{code}

We now look at expansions based on random-numbers:
\begin{code}
numexpanders = [alu,aru,mlu,mru,sru]
expcount = length numexpanders
numexpander i = numexpanders!!i  -- i in 0..(length numexpanders)-1
rexpand :: [Int]          -- random number list
           -> Int         -- recurse depth
           -> HE          -- expression
           -> (HE,[Int])  -- expanded expression, remaining random numbers
rexpand rns 0 he = (he,rns) 
rexpand [] lvl he = error ("rexpand: not enough random nos. for "++hask he)
rexpand (rn:rns) lvl he
  = case numexpander rn he of
      (B op he1 he2) 
        ->  let (he1',rns1) = rexpand rns lvl' he1
                (he2',rns2) = rexpand rns1 lvl' he2
            in  ( B op he1' he2' , rns2)
      (A f hes)
        ->  let (hes',rns') = rexpands rns lvl' [] hes
            in  ( A f hes' , rns')  
      he' -> (he',rns)
  where lvl' = lvl-1

rexpands rns lvl seh [] = ( reverse seh, rns )
rexpands rns lvl seh (he:hes)
  = let (he',rns') = rexpand rns lvl he
    in  rexpands rns' lvl (he':seh) hes

twistexpand rns (he1,he2)
  = let
      (he1',rns') = rexpand rns 2 he1
      (he2',rns'') = rexpand rns' 2 he2
    in ((he1',he2'),rns'')
\end{code}

We have a constraint that we have a fixed number of expansion laws.

We define a pretty printer producing Haskell syntax:
\begin{code}
hask,wrap :: HE -> String
hask (N i) = show i
hask (V s) = s
hask E = "[]"
hask (C x xs) = wrap x++":"++wrap xs
hask (B op he1 he2) = wrap he1 ++ " " ++ op ++ " " ++ wrap he2
hask (A f hes) = f ++ " " ++ intercalate " " (map wrap hes)
wrap he@(N _) = hask he
wrap he@(V _) = hask he
wrap he@E     = hask he
wrap he = "("++hask he++")"
hask2 :: (HE,HE) -> String
hask2 ( he1, he2 ) = hask he1 ++ " == " ++ hask he2
\end{code}

We are going to use the four add/sub/mul twist laws as initial kernels.
We then complexify 2 levels deep.
\begin{code}
sub_add_twist
  = ( (vx `sub` vy) `sub` vz  -- (x-y)-z
    ,                         --  =
      vx `sub` (vz `add` vy)  -- v-(z+y)
    )
add_sub_twist
  = ( vx `sub` (vy `sub` vz)  -- x-(y-z)
    ,                         --  =
      (vz `add` vx) `sub` vy  -- (z+x)-y
    )
mul_sub_twist
  = ( vx `mul` (vy `sub` vz)  -- x*(y-z)
    ,                         --  =
      (vy `sub` vz) `mul` vx  -- (y-z)*y
    )
mul_add_twist
  = ( vx `mul` (vy `add` vz)  -- x*(y+z)
    ,                         --  =
      (vy `add` vz) `mul` vx  -- (y+z)*y
    )
twists = [sub_add_twist,add_sub_twist,mul_sub_twist,mul_add_twist]
\end{code}


\paragraph{Question 2}~

\begin{code}
h cmp stop base op drop x
  = if (x `cmp` stop)
    then base
    else x `op` h cmp stop base op drop (drop x)

fac = h (==) 0 1 (*) (subtract 1)
summ = h (==) 0 0 (+) (subtract 1)
fac2 = h (<=) 0 1 (*) (subtract 2)
summ2 = h (<=) 0 0 (+) (subtract 2)

facdiv = h (<=) 1 1 (*) (`div` 2)



facstr = ("0","1","*","- 1")
sumstr = ("0","0","+","- 1")
fac2str = ("0","1","*","- 2")
sum2str = ("0","0","+","- 2")
facdivstr = ("1","1","*","`div` 2")
writerec fnm (stop, base, op, drop)
  = unwords
      [ fnm ++ " x  = "
      , "if x <= "++stop
      , "then "++base
      , "else x "++op++" "++fnm
      , "(x "++drop++")"
      ]
\end{code}

\begin{code}
recFNm = "frec"

randRecFn :: Int -> Int -> Int -> Int -> String
randRecFn 0 stop base drop 
  = writerec "frec" (show stop, show base,"+","- "++show drop)
randRecFn 1 stop base drop
  = writerec "frec" (show stop, show base,"*","`div` "++show drop)

randRecFnArg :: Int -> Int -> Int -> Int -> Int
randRecFnArg 0 stop base drop = stop + 2*drop
randRecFnArg 1 stop base drop = stop*drop*drop

randRecFnCall :: Int -> Int -> Int -> Int -> String
randRecFnCall i stop base drop
  = "frec "++ show (randRecFnArg i stop base drop)

randRecFnRes :: Int -> Int -> Int -> Int -> String
randRecFnRes 0 stop base drop 
 = show $ recf (+) stop base (subtract drop) $ randRecFnArg 0 stop base drop
randRecFnRes 1 stop base drop 
 = show $ recf (*) stop base (`div` drop) $ randRecFnArg 1 stop base drop

recf :: (Int->Int->Int) -> Int -> Int -> (Int->Int) -> Int -> Int
recf op stop base drop x
  = if x <= stop then base else x `op` f (drop x)
  where f = recf op stop base drop
\end{code}

\paragraph{Question 3}~

\begin{code}
bonusFn b1 b2
  = unlines
     [ "bonus []      =  "++show b1
     , "bonus (x:xs)  =  x + "++show b2++" + bonus xs"
     ]

bonusProp b1 b2 
  = "bonus xs == "++show b1++" + "++show b2++"*(len xs) + sum xs"
\end{code}

\paragraph{Question 4}~

\begin{code}
caseF theN
  = unlines
      [ "casef x"
      , "  | x < "++show theN++"   =  2*x"
      , "  | x >= "++show theN++"  = 2*x-1"
      ]

caseProp _
  = "casef (x+1) > casef x"

theCaseScheme theN
  = [ "CASE-SCHEME Plus1Order"
    , "CASE 1  "++case1
    , "CASE 2  "++case2
    , "CASE 3  "++case3
    , "CASE 4  "++case4
    , "EXHAUSTIVE " ++ case1 
           ++" || " ++ case2
           ++" || " ++ case3
           ++" || " ++ case4
    , "EXCLUSIVE 1 2  not("++case1++" && "++case2++")"
    , "EXCLUSIVE 1 3  not("++case1++" && "++case3++")"
    , "EXCLUSIVE 1 4  not("++case1++" && "++case4++")"
    , "EXCLUSIVE 2 3  not("++case2++" && "++case3++")"
    , "EXCLUSIVE 2 4  not("++case2++" && "++case4++")"
    , "EXCLUSIVE 3 4  not("++case3++" && "++case4++")"
    ]
  where
   case1 = "(x <  " ++ show(theN-1) ++ ")"
   case2 = "(x == " ++show(theN-1)  ++ ")"
   case3 = "(x == "++show theN      ++ ")"
   case4 = "(x >  "++show theN      ++ ")"
\end{code}

\newpage
\subsubsection{Test Generation}

\begin{code}
generateTests5 :: Config -> IO ()
generateTests5 config
  = do putStr $ showConfig config
       putStrLn "Generating T5"
       let sg0 = mkStdGen $ read $ idno config
       sg <- genTst5 config sg0
       return ()
\end{code}


\begin{code}
genTst5 :: Config -> StdGen -> IO StdGen
genTst5 config sg 
  = do ([ex5rn1,ex5rn2,ex5rn3],sg) <- genExercise5RNs sg

       putStrLn ("Ex5 T1 rn1 = "++show ex5rn1)
       (suppt1,t1) <- specifyT1 mname ex5rn1 
       putStrLn ("Ex5 T2 rn2 = "++show ex5rn2)
       (suppt2,t2) <- specifyT2 mname ex5rn2
       putStrLn ("Ex5 T3 rn3 = "++show ex5rn3)
       (suppt3,t3) <- specifyT3 mname ex5rn3

       let support = concat $ intersperse "\n" 
                       [suppt1,suppt2,suppt3]
       let tests = concat $ intersperse "\n" [t1,t2,t3]
       writeFile fname 
         ( specPreamble 
              "import Test5Support" theExerciseNumber 
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
\subsubsection{Ex 5 Q 1}

\ExFiveQI

\begin{code}
suppliedQ1 :: [Int] -> String
suppliedQ1 rn1 = "-- no code for Q1"
\end{code}


\begin{code}
specifyQ1 :: [Int] -> IO String
specifyQ1 (twsti:expanders)
  = return $ unlines 
      ( exercisePreamble "" theExerciseNumber 1
        :
        [ "   " ++ hask2 randomTwist, ""]
        ++
        [exercisePostamble theExerciseNumber 1] )
  where
    theTwist = twists !! twsti
    (randomTwist,_) = twistexpand expanders theTwist
\end{code}

\begin{code}
specifyT1 :: String -> [Int] -> IO (String, String)
specifyT1 mname rn1 
  = return 
     ( supportT1 rn1
     , 
       unlines 
        [ testGroupName theExerciseNumber 1
          ++ " = testGroup \"\\nQ1 ("++show ex5q1mark++" marks)\" ["
        , "  ]"
        ] 
      )


supportT1 :: [Int] -> String
supportT1 rn1 = unlines 
  [ hc "EX 5 T1 support contained in Test5Support"
  ]
\end{code}


\newpage
\subsubsection{Ex 5 Q 2}

\ExFiveQII

\begin{code}
suppliedQ2 :: [Int] -> String
suppliedQ2 [aom,stop,base,drop] 
  = unlines
      [ "-- for Q2:"
      , randRecFn aom stop base drop ]
\end{code}


\begin{code}
specifyQ2 :: [Int] -> IO String
specifyQ2 [aom,stop,base,drop]
  = return $ unlines 
      ( exercisePreamble "" theExerciseNumber 2
        :
        [ "   " 
          ++ randRecFnCall aom stop base drop 
          ++ " == "
          ++ randRecFnRes  aom stop base drop
        , ""]
        ++
        [exercisePostamble theExerciseNumber 2] )
\end{code}

\begin{code}
specifyT2 :: String -> [Int] -> IO (String, String)
specifyT2 mname rn2 
  = return 
     ( supportT2 rn2
     , 
       unlines 
        [ testGroupName theExerciseNumber 2
          ++ " = testGroup \"\\nQ2 ("++show ex5q2mark++" marks)\" ["
        , "  ]"
        ] 
      )

supportT2 :: [Int] -> String
supportT2 [foldlorr,tupleorder,tuple,mtype,ntype] = unlines 
  [ 
  ]
\end{code}


\newpage
\subsubsection{Ex 5 Q 3}

\ExFiveQIII


\begin{code}
suppliedQ3 :: [Int] -> String
suppliedQ3 [b1,b2] 
  = unlines 
      [ "-- for Q3:"
      , bonusFn b1 b2 ]
\end{code}


\begin{code}
specifyQ3 :: [Int] -> IO String
specifyQ3 [b1,b2] 
  = return $ unlines 
      ( exercisePreamble "" theExerciseNumber 3
        :
        [ "   " ++ bonusProp b1 b2, ""]
        ++
        [exercisePostamble theExerciseNumber 3] )
\end{code}

\begin{code}
specifyT3 :: String -> [Int] -> IO (String, String)
specifyT3 mname rn3 
  = return 
     ( supportT3 rn3
     , 
       unlines 
        [ testGroupName theExerciseNumber 3
          ++ " = testGroup \"\\nQ3 ("++show ex5q3mark++" marks)\" ["
        , "  ]"
        , hc $ show rn3
        ] 
      )

supportT3 :: [Int] -> String
supportT3 rn3 = unlines 
  [ "-- EX5 T3 support contained in Test3Support"
  ]
\end{code}


\newpage
\subsubsection{Ex 5 Q 4}

\ExFiveQIV


\begin{code}
suppliedQ4 :: [Int] -> String
suppliedQ4 [theN] 
  = unlines
      [ "--for Q4:"
      , caseF theN ]
\end{code}


\begin{code}
specifyQ4 :: [Int] -> IO String
specifyQ4 [theN] 
  = return $ unlines 
      ( exercisePreamble "" theExerciseNumber 4
        :
        [ "   " ++ caseProp theN, ""]
        ++
        [exercisePostamble theExerciseNumber 4]
        ++
        theCaseScheme theN )
\end{code}

\begin{code}
specifyT4 :: String -> [Int] -> IO (String, String)
specifyT4 mname rn4 
  = return 
     ( supportT4 rn4
     , 
       unlines 
        [ testGroupName theExerciseNumber 4
          ++ " = testGroup \"\\nQ4 ("++show ex5q4mark++" marks)\" ["
        , "  ]"
        , hc $ show rn4
        ] 
      )

supportT4 :: [Int] -> String
supportT4 rn4 = unlines 
  [ "-- EX5 T4 support contained in Test4Support"
  ]
\end{code}


