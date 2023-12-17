\subsection{Configuration}

\begin{code}
module 
  Configuration 
    ( Config(..), config0
    , setExerciseNumber
    , loadConfig, saveConfig
    , showUser, showConfig
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
\end{code}


\begin{code}
data Config 
  = Config { exno :: Int
           , name, idno, username :: String }
  deriving (Show, Read)

config0 = Config 0 "?" "?" "?"

setExerciseNumber :: Int -> Config -> Config
setExerciseNumber n config = config{ exno = n }

saveConfig filename config
  = writeFile filename $ show config

loadConfig filename
  = do txt <- readFile filename
       let cfg = read txt
       return cfg

showUser config 
    = unlines
       [ "Name         : "  ++ name config 
       , "Student Id   : "  ++ idno config 
       , "TCD Username : "  ++ username config 
       ]

showConfig config 
    = ("Exercise No. " ++ show (exno config)++"\n") ++ showUser config
\end{code}
