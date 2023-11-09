\subsection{Random Generation for Exercises}

\begin{code}
module 
  RandomExercises(
    genExercise1RNs
  , genExercise2RNs
  , genExercise3RNs
  )
where

import System.Random
import Randomisation
\end{code}

\subsubsection{Introduction}

Here we provide random-number sequences tailored for each exercise.
An exercise has a given number of questions, 
each of which has a given number of random numbers.

\begin{code}
type ExerciseNumbers = [[Int]]
type ExerciseGen = StdGen -> IO (ExerciseNumbers,StdGen)
\end{code}

% \newpage
\subsubsection{Exercise 1}

\input{exercises/Exercise-1-Plan}

\begin{code}
genExercise1RNs :: ExerciseGen
genExercise1RNs sg
  =  do (n,sg) <- doRandomR (10000000,99999999) sg
        return ([[n]],sg)
\end{code}

\newpage
\subsubsection{Exercise 2}

\input{exercises/Exercise-2-Plan}

\begin{code}
genExercise2RNs :: ExerciseGen
genExercise2RNs sg
  =  do (i,sg) <- doRandomR (100,199) sg
        (j,sg) <- doRandomR (200,299) sg
        (k,sg) <- doRandomR (300,399) sg

        (opcodes,sg) <- doRandomRUn 12 (10,99) sg

        (fixedadd,sg) <- doRandomRn 3 (3,6) sg
        (fixedaddstop,sg) <- doRandomR (0,9) sg
        (stopadd,sg) <- doRandomRn 3 (3,6) sg
        (stopaddstop,sg) <- doRandomR (0,9) sg
        let addsettings = fixedadd++[fixedaddstop]++stopadd++[stopaddstop]
 
        (fixedmul,sg) <- doRandomRn 3 (3,6) sg
        (fixedmulstop,sg) <- doRandomR (0,9) sg
        (stopmul,sg) <- doRandomRn 3 (3,6) sg
        (stopmulstop,sg) <- doRandomR (0,9) sg
        let mulsettings = fixedmul++[fixedmulstop]++stopmul++[stopmulstop]
 
        let ell = opcodes++addsettings++mulsettings
        return ([[i],[j],[k],ell],sg)
\end{code}


% \newpage
\subsubsection{Exercise 3}

\input{exercises/Exercise-3-Plan}

\begin{code}
genExercise3RNs :: ExerciseGen
genExercise3RNs sg0
  =  do (ex,sg1)  <- doRandomR (0,3) sg0 -- Expr(4)
        (val,sg2) <- doRandomR (0,2) sg1 -- Val(3)
        (var,sg3) <- doRandomR (0,3) sg2 -- Var(4)
        (dvd,sg4) <- doRandomR (0,3) sg3 -- DvD(4)
        (binN,sg5) <- doRandomR (0,2) sg4 -- Add, Sub, Mul
        (binNV,sg6) <- doRandomR (0,2) sg5 -- 3 variants
        (uniN,sg7) <- doRandomR (0,1) sg6 -- Abs Neg
        (uniNV,sg8) <- doRandomR (0,2) sg7 -- 3 variants
        (binB,sg9) <- doRandomR (0,3) sg8 -- Eq Neq Less Greater
        (binBV,sg10) <- doRandomR (0,2) sg9 -- 3 variants
        (uniB,sg11) <- doRandomR (0,1) sg10 -- Zero NonZero
        (uniBV,sg12) <- doRandomR (0,2) sg11 -- 3 variants
        let ex3rn = [ ex, val, var, dvd
                     , binN, binNV, uniN, uniNV
                     , binB, binBV, uniB, uniBV ]
        return ([ex3rn],sg12)
\end{code}

% \begin{code}
% \end{code}

% \begin{code}
% \end{code}

% \begin{code}
% \end{code}

% \begin{code}
% \end{code}