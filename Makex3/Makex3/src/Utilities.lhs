\section{Utilities}
\begin{code}
module Utilities where

import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO
import Data.List
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Lazy.UTF8 as BU
\end{code}
\codesection{putLblXXXLn}

\begin{code}
putLblStrLn :: String -> String -> IO ()
putLblStrLn lbl str = putStrLn ( lbl ++ str )

putLblValueLn :: Show a => String -> a -> IO ()
putLblValueLn lbl thing = putStrLn ( lbl ++ show thing )
\end{code}


\codesection{prepareEmptyDirectory}

We want an empty directory into which
we will unpack all the submissions
\begin{code}
prepareEmptyDirectory unpack
  = do alreadyPresent <- doesDirectoryExist unpack
       ifthen alreadyPresent $ removeDirectoryRecursive unpack
       createDirectory unpack
\end{code}



\codesection{blockfilter}

Return blocks starting with a line satisfying predicate\verb"p"
A block is a maximal run of non-empty lines.
\begin{code}
blockfilter _ [] = []
blockfilter p (ln:lns)
 | p ln = bf [ln] lns
 | otherwise =  blockfilter p lns
 where
   bf blns [] = reverse blns
   bf blns (ln:lns)
    | null ln  =  reverse blns ++ ("":blockfilter p lns)
    | otherwise  =  bf (ln:blns) lns
\end{code}


\codesection{localise}

\begin{code}
localise :: FilePath -> IO a -> IO a
localise relfp proc
  = do cwd <- getCurrentDirectory
       setCurrentDirectory (cwd </> relfp)
       ret <- proc
       setCurrentDirectory cwd
       return ret
\end{code}

\codesection{xIfThen}

\begin{code}
mifthen :: Monad m => a -> Bool -> m a -> m a
mifthen def b act
 | b          =  act
 | otherwise  =  return def

ifthen :: Monad m => Bool -> m () -> m ()
ifthen = mifthen ()
\end{code}


\subsection{Return Codes}

\begin{code}
rcOK ExitSuccess  =  True
rcOK _            =  False

rcBad (ExitFailure _)  =  True
rcBad _                =  False

continueif rc act
 | rcOK rc    =  act
 | otherwise  =  return rc
\end{code}

\subsection{Grammatical number rendering}

Want to 1st, 2nd, 3rd, 4th, \dots\ suffixes properly:
\begin{code}
showth :: Int -> String
showth n = nstr ++ genth nstr
  where
    nstr = show n

genth = gth . reverse

gth (_:'1':_) = "th"  -- ..11th, ..12th, ...
gth ('1':_) = "st"
gth ('2':_) = "nd"
gth ('3':_) = "rd"
gth _       = "th"
\end{code}

\subsection{Odds and Sods}

\begin{code}
ttail [] = []
ttail (_:xs) = xs

readInt :: String -> Int
readInt "" = 0
readInt str = read str

prompt string = do { putStr string ; hFlush stdout }

getResponse = do txt <- getLine
                 return $ trim txt

putStrLns = putStrLn . unlines

trim = reverse . ltrim . reverse . ltrim
ltrim "" = ""
ltrim str@(c:cs)
  | isSpace c  =  ltrim cs
  | otherwise  =  str
\end{code}
