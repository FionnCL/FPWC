\subsection{Exercise One}

\begin{code}
module 
  Exercise1( 
    theExerciseNumber, exerciseFolder
  , generateExercise1, generateTests1 
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
theExerciseNumber = (1 :: Int)
exerciseFolder = "Exercise"++show theExerciseNumber
\end{code}

\subsubsection{Exercise Generation}

\begin{code}
generateExercise1 :: Config -> IO ()
generateExercise1 config
  = do putStr $ showConfig config
       putStrLn "Generating Q1"
       let sg0 = mkStdGen $ read $ idno config
       sg <- genQ1 config sg0
       return ()
\end{code}


\subsubsection{Question One}

\begin{code}
genQ1 :: Config -> StdGen -> IO StdGen
genQ1 config sg 
  = do ([[n]],sg) <- genExercise1RNs sg
       putStrLn ("Ex1 Q1 n = "++show n)
       q1 <- specifyQ1 mname n
       writeFile fname q1
       -- genTestsQ1 n
       return sg
  where
    nn = show $ exno config
    mname = "Ex"++nn
    fname = exerciseFolder++"/src/"++mname++".hs"
\end{code}

\begin{code}
specifyQ1 :: String -> Int -> IO String
specifyQ1 mname n 
  = return $ unlines $
      [ "module "++mname++" where"
      , "-- Q1"
      , "f1 :: Int -> Int"
      , "-- adds "++show n++" to its input"
      , "f1 ns = undefined"
      ]
\end{code}

\subsubsection{Test Generation}

\begin{code}
generateTests1 :: Config -> IO ()
generateTests1 config
  = do putStr $ showConfig config
       putStrLn "Generating T1"
       let sg0 = mkStdGen $ read $ idno config
       sg <- genT1 config sg0
       return ()
\end{code}



\subsubsection{Test One}

\begin{code}
genT1 :: Config -> StdGen -> IO StdGen
genT1 config sg 
  = do ([[n]],sg) <- genExercise1RNs sg
       putStrLn ("Ex1 T1 n = "++show n)
       t1 <- specifyT1 mname n
       writeFile fname t1
       return sg
  where
    nn = show $ exno config
    mname = "Spec"
    fname = exerciseFolder++"/test/"++mname++".hs"
\end{code}

\begin{code}
specifyT1 :: String -> Int -> IO String
specifyT1 mname n 
  = return 
     ( specPreamble "" theExerciseNumber
       ++ 
       unlines 
        [ "tests = [ testGroup \"TEST Exercise 01 (10 marks)\""
        , "          [ testCase \"Q1 T1 [10 marks]\" "
        , "            ( f1 11111111 @?= 11111111 + "++show n++")"
        , "          ]"
        , "        ]"
        ] )
\end{code}
