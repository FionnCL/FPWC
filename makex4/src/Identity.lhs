\subsection{Student Identity}

\begin{code}
module Identity( getIdentity ) where

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
\end{code}



\begin{code}
getIdentity :: Config -> IO Config
getIdentity config
  = do config1 <- getName config
       config2 <- getIdNo config1
       config3 <- getUsername config2
       putStrLn $ showUser config3
       putStrLn "OK?  [y/N] -- default is 'N' (No)"
       response <- getResponse
       if map toLower response `elem` ["y","yes"]
       then return config3
       else getIdentity config
\end{code}

Student Name
\begin{code}
getName :: Config -> IO Config
getName config
  = do prompt "Enter your name > " ; hFlush stdout
       response <- getResponse
       putStrLn ("You entered: "++response)
       return config{name = response}
\end{code}

Student Id Number
\begin{code}
getIdNo :: Config -> IO Config
getIdNo config
  = do prompt "Enter your Student Id Number (8 digits) > "
       response <- getResponse
       putStrLn ("You entered: "++response)
       if validIDNo response
       then return config{idno = response}
       else do putStrLn "Expected exactly 8 decimal digits"
               getIdNo config

validIDNo str = all isDigit str && length str == 8
\end{code}

Student Username
\begin{code}
shortest = 3
longest = 9
getUsername :: Config -> IO Config
getUsername config
  = do prompt "Enter your College Username > "
       response <- getResponse
       putStrLn ("You entered: "++response)
       if validUsername response
       then return config{username = response}
       else do putStrLn $ unlines
                [ "First character must be a letter"
                , "Expected between "
                   ++show shortest++
                   " and "
                   ++show longest++
                   " letters and numbers"
                ]
               getUsername config

validUsername str 
   = (length str `elem` [shortest..longest]) && 
     isAlpha (head str) &&
     all validUserChar (tail str)
validUserChar c = isAlpha c || isDigit c
\end{code}

