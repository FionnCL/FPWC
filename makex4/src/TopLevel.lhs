\subsection{Common Top-Level Functionality}

\begin{code}
module 
  TopLevel ( configFile, getConfig )
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
\end{code}


\subsubsection{Configuration}

The first time the program is run,
it requests student details, with some checking.
This produces a config file (\texttt{config.ini}) 
that is then loaded for subsequent usage.
\begin{code}
configFile = "makex.conf"

getConfig True cf = loadConfig cf
getConfig False cf
  = do  config <- getIdentity config0
        saveConfig cf config
        return config
\end{code}

