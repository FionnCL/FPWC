\subsection{Make Exercise Four}

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
import Exercise4
\end{code}

\begin{code}
progname = "makex4"
\end{code}

\subsubsection{Description}

This generates Exercise 4

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

Two things are distributed: this program,
and an exercise folder missing the \texttt{ExNN.hs} file.
This program is built, and then run from within the parent folder 
of the exercise folder.


\subsubsection{Mainline}

\begin{code}
main
  = do  have_config <- doesFileExist configFile
        config <- getConfig have_config configFile
        let config' = setExerciseNumber 4 config
        generateExercise4 "Exercise4" config'
\end{code}


