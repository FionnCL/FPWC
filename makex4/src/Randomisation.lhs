\subsection{Random Number Generators}

\begin{code}
module 
  Randomisation 
    ( randomRn, doRandomR, doRandomRn, randomRUn, doRandomRUn )
where

import System.Random
\end{code}

\subsubsection{Monadic random number}

\begin{code}
doRandomR :: (Monad m, Random a, RandomGen g) => (a, a) -> g -> m (a, g)
doRandomR (lo,hi) g = return $ randomR (lo,hi) g
\end{code}


\subsubsection{Random number list}

\begin{code}
randomRn :: Random a => Int -> (a,a) -> StdGen -> ([a],StdGen)
randomRn n (lo,hi) g  =  randomRn' (lo,hi) [] g n

randomRn' (lo,hi) sr g 0 = (reverse sr,g)
randomRn' (lo,hi) sr g n
  = let
      (r',g') = randomR (lo,hi) g
    in  randomRn' (lo,hi)  (r':sr) g' (n-1)

doRandomRn :: (Monad m, Random a) => Int -> (a, a) -> StdGen -> m ([a], StdGen)
doRandomRn n (lo,hi) g = return $ randomRn n (lo,hi) g
\end{code}

\subsubsection{Random unique number list}

\begin{code}
randomRUn :: (Eq a, Random a) => Int -> (a,a) -> StdGen -> ([a],StdGen)
randomRUn n (lo,hi) g  =  randomRUn' (lo,hi) [] g n

randomRUn' (lo,hi) sr g 0 = (reverse sr,g)
randomRUn' (lo,hi) sr g n
  = let
      (r',g') = randomR (lo,hi) g
    in if r' `elem` sr then randomRUn' (lo,hi)  sr g' n -- try again
                       else randomRUn' (lo,hi)  (r':sr) g' (n-1)

doRandomRUn :: (Monad m, Eq a, Random a) 
            => Int -> (a, a) -> StdGen -> m ([a], StdGen)
doRandomRUn n (lo,hi) g = return $ randomRUn n (lo,hi) g
\end{code}


