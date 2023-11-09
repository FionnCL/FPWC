\subsection{Test Exercise Three}

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
import Exercise3
\end{code}

\begin{code}
progname = "testex3"
\end{code}

\subsubsection{Description}


The student id-number is used as a random generator seed.
The random generator is used to specialise a specification template
tailored for a given exercise.

The specification then produces:
\begin{itemize}
  \item
    instructions for the exercise
  \item
    boilerplate and initial code
  \item
    tests for the exercise
\end{itemize}
These are then used to build a fully formed Exercise folder.

This program is built, 
and then run using \texttt{stack test} to get output for grading.


\subsubsection{Mainline}

\begin{code}
main
  = do  have_config <- doesFileExist configFile
        config <- getConfig have_config configFile
        let config' = setExerciseNumber 3 config
        generateExercise3 "Exercise3" config'
\end{code}


