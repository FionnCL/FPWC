\subsection{Make Exercise Five}

\begin{code}
module Main where

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
import Exercise5
\end{code}

\begin{code}
progname = "makex5"
\end{code}

\subsubsection{Description}

This generates Exercise 5

The student id-number is used as a random generator seed.
The random generator is used to specialise a specification template
tailored for a given exercise.

The specification then produces:
\begin{itemize}
  \item
    instructions for the exercise
  \item
    boilerplate and initial code
\end{itemize}
 The these are then used to build a fully formed Exercise folder.

Three things are distributed: the \texttt{prfchk} program,
this program,
and an \texttt{theories} folder with some useful theories.
This program is run from within the theories folder,
where it adds in one theory file for each question 
(\texttt{Ex5Qn.thr} for \texttt{n} in 1 \dots 4),
and support haskell code in \texttt{Ex5.hs}


\subsubsection{Mainline}

\begin{code}
main
  = do  have_config <- doesFileExist configFile
        config <- getConfig have_config configFile
        let config' = setExerciseNumber 5 config
        generateExercise5 "." config'
\end{code}


