\subsection{Random Generation for Exercises}

\begin{code}
module 
  RandomExercises(
    genExercise1RNs
  , genExercise2RNs
  , genExercise3RNs
  , genExercise4RNs
  , genExercise5RNs
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

\newpage
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


\newpage
\subsubsection{Exercise 3}

\input{exercises/Exercise-3-Plan}

\begin{code}
genExercise3RNs :: ExerciseGen
genExercise3RNs sg
  =  do (ex,sg)  <- doRandomR (0,3) sg -- Expr(4)
        (val,sg) <- doRandomR (0,2) sg -- Val(3)
        (var,sg) <- doRandomR (0,3) sg -- Var(4)
        (dvd,sg) <- doRandomR (0,3) sg -- DvD(4)
        (binN,sg) <- doRandomR (0,2) sg -- Add, Sub, Mul
        (binNV,sg) <- doRandomR (0,2) sg -- 3 variants
        (uniN,sg) <- doRandomR (0,1) sg -- Abs Neg
        (uniNV,sg) <- doRandomR (0,2) sg -- 3 variants
        (binB,sg) <- doRandomR (0,3) sg -- Eq Neq Less Greater
        (binBV,sg) <- doRandomR (0,2) sg -- 3 variants
        (uniB,sg) <- doRandomR (0,1) sg -- Zero NonZero
        (uniBV,sg) <- doRandomR (0,2) sg -- 3 variants
        let ex3rn = [ ex, val, var, dvd
                     , binN, binNV, uniN, uniNV
                     , binB, binBV, uniB, uniBV ]
        return ([ex3rn],sg)
\end{code}

\newpage
\subsubsection{Exercise 4}

\input{exercises/Exercise-4-Plan}

\begin{code}
genExercise4RNs :: ExerciseGen
genExercise4RNs sg
  =  do (ex,sg)  <- doRandomR (0,3) sg -- Expr(4)
        (val,sg) <- doRandomR (0,2) sg -- Val(3)
        (var,sg) <- doRandomR (0,3) sg -- Var(4)
        (dvd,sg) <- doRandomR (0,3) sg -- DvD(4)
        (binN,sg) <- doRandomR (0,2) sg -- Add, Sub, Mul
        (binNV,sg) <- doRandomR (0,2) sg -- 3 variants
        (uniN,sg) <- doRandomR (0,1) sg -- Abs Neg
        (uniNV,sg) <- doRandomR (0,2) sg -- 3 variants
        (binB,sg) <- doRandomR (0,3) sg -- Eq Neq Less Greater
        (binBV,sg) <- doRandomR (0,2) sg -- 3 variants
        (uniB,sg) <- doRandomR (0,1) sg -- Zero NonZero
        (uniBV,sg) <- doRandomR (0,2) sg -- 3 variants
        let ex4rn1 = [ ex, val, var, dvd
                     , binN, binNV, uniN, uniNV
                     , binB, binBV, uniB, uniBV ]

        (foldlorr,sg) <- doRandomR (0,1) sg -- foldl,foldr
        (tupleorder,sg) <- doRandomR (0,1) sg -- (z,op) vs. (op,z)
        (tuple,sg) <-  doRandomR (0,2) sg  -- (m,[n]); ([m],n); ([m],[n])
        (mtype,sg) <-  doRandomR (0,2) sg  -- Bool, Int, Float
        (ntype,sg) <-  doRandomR (0,2) sg  -- Bool, Int, Float
        let ex4rn2 = [foldlorr,tupleorder,tuple,mtype,ntype]

        (nooffuns,sg) <- doRandomR (9,13) sg -- no. of. functions
        (sectypes,sg) <- doRandomRn nooffuns (0,3) sg -- +,*,-k,k-
        (secval,sg) <- doRandomRn nooffuns (19,31) sg -- +,*,-k,k-
        let ex4rn3 = nooffuns:sectypes++secval
        return ([ex4rn1,ex4rn2,ex4rn3],sg)
\end{code}

\newpage
\subsubsection{Exercise 5}

\input{exercises/Exercise-5-Plan}

\begin{code}
genExercise5RNs :: ExerciseGen
genExercise5RNs sg
  =  do (twstid,sg)  <- doRandomR (0,3) sg
        (expandRns,sg) <- doRandomCycle 2 4 (0,4) sg  
        let ex5rn1 = twstid:expandRns
        (aom,sg) <- doRandomR (0,1) sg  -- 0 additive, 1 multiplicative
        (stop,sg) <- doRandomR (3,8) sg 
        (base,sg) <- doRandomR (4,9) sg 
        (drop,sg) <- doRandomR (5,10) sg 
        let ex5rn2 = [aom,stop,base,drop]
        (b1,sg) <- doRandomR (1,9) sg  
        (b2,sg) <- doRandomR (1,9) sg  
        let ex5rn3 = [2*b1,2*b2+1]
        (theN,sg) <- doRandomR (3,10) sg  
        let ex5rn4 = [theN]
        return ([ex5rn1,ex5rn2,ex5rn3,ex5rn4],sg)
\end{code}

% \begin{code}
% \end{code}

% \begin{code}
% \end{code}

% \begin{code}
% \end{code}

% \begin{code}
% \end{code}