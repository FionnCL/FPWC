
\input{Copyright}
\begin{code}
module Main where

import Data.List
import Data.Maybe

import System.Environment
import System.Directory
import System.FilePath
import Control.Exception

import Utilities
import REPL
import AST
import Matching
import HParse
import Theory
import Check
import Debug.Trace
dbg msg x = trace (msg ++ show x) x
pdbg nm x = dbg (nm++":\n") x
mdbg msg x = return $! dbg msg x
\end{code}

\begin{code}
version = "1.0.0.0"
\end{code}

\chapter{Main Program}

\section{Program State}

\begin{code}
data HReqState
  = HReq { wkdir :: FilePath
         , hmods :: [Mdl]
         , hthrys :: [Theory]
         , currThry :: Maybe Theory
         }
  deriving Show

wkdir__ f hrs = hrs{ wkdir = f $ wkdir hrs} ; wkdir_ h = wkdir__ $ const h
hmods__ f hrs = hrs{ hmods = f $ hmods hrs} ; hmods_ h = hmods__ $ const h
hthrys__ f hrs = hrs{ hthrys = f $ hthrys hrs} ; hthrys_ h = hthrys__ $ const h
currThry__ f hrs = hrs{ currThry = f $ currThry hrs}
currThry_ h = currThry__ $ const h

hreqs0 = HReq "" [] [] Nothing

setWD :: FilePath -> HReqState -> (FilePath,HReqState)
setWD fp hreqs 
  = ( filename, wkdir_ pathtodir hreqs )
  where (pathtodir,filename) = splitFileName fp

getWD :: HReqState -> FilePath -> FilePath
getWD hreqs filename  =  wkdir hreqs </> filename
\end{code}

\section{Program Mainline}

\begin{code}
main :: IO ()
main
 = do args <- getArgs
      case args of
        []    ->  repl
        [fp]  ->  batch fp
        _     ->  showUsage
               
showUsage = putStrLn $ unlines 
  [ "usage: prfchk [filepath]"
  , " filepath is of a theory file"
  , " (default extension '.thr')"
  , "If no filepath given, the command line interface runs"
  , "If filepath is given,"
  , " that theory is loaded and all theorems are checked"
  ]
\end{code}

\newpage
\section{Batch Processing}

\begin{code}
batch :: FilePath -> IO ()
batch fpath
  = do putStrLn ("\n\t****\n\tRunning Proof Check v"++version)
       let (fname, hreqs) = setWD fpath hreqs0
       putStrLn ("Working directory: "++wkdir hreqs)
       hreqs <- loadTheory fname hreqs
       case currThry hreqs of
         Nothing    -> putStrLn ( "Failed to load "++fpath++".thr")
         Just thry
           -> do let pc = PC (hmods hreqs) (hthrys hreqs ++ [thry]) []
                 putStrLn "STARTING BATCH CHECK...\n"
                 sequence_ $ map (showReport  . checkTheorem pc) 
                           $ thTheorems thry
                 putStrLn "\nFINISHED BATCH CHECK"
\end{code}

\section{Read-Eval-Print-Loop}

\begin{code}
repl :: IO ()
repl
  = do runREPL hreqWelcome hreqConfig hreqs0
       return ()

hreqWelcome = unlines
  [ "Welcome to Proof Check v"++version
  , "To run in batch mode, give name of theory file when invoking from shell."
  , "Type '?' for help."
  ]
\end{code}

\subsection{REPL Configuration}

Types:
\begin{code}
type HReqCmd       =  REPLCmd      HReqState
type HReqCmdDescr  =  REPLCmdDescr HReqState
type HReqExit      =  REPLExit     HReqState
type HReqCommands  =  REPLCommands HReqState
type HReqConfig    =  REPLConfig   HReqState
\end{code}

\newpage

REPL Basics:
\begin{code}
hreqPrompt :: Bool -> HReqState -> String
hreqPrompt _ _ = "prfchk> "

hreqEOFreplacmement = [nquit]

hreqParser = wordParse

hreqQuitCmds = [nquit] ; nquit = "q"

hreqQuit :: HReqExit
hreqQuit _ hreqs = putStrLn "\nGoodbye!\n" >> return (True, hreqs)

hreqHelpCmds = ["?"]

-- we don't use these features in the top-level REPL
hreqEndCondition _ = False
hreqEndTidy _ hreqs = return hreqs
\end{code}

\section{REPL Commands}

\subsection{Displaying Prover State}

\begin{code}
cmdShowState :: HReqCmdDescr
cmdShowState
  = ( "state"
    , "show state"
    , "show short summary of state contents"
    , showState )

showState _ hreqs
  = do putStrLn ""
       showHModNames   $ hmods    hreqs
       showTheoryNames $ hthrys   hreqs
       showCurrThry    $ currThry hreqs
       showCWD         $ wkdir    hreqs
       putStrLn ""
       return hreqs

showHModNames [] = putStrLn "No Haskell Modules"
showHModNames hms = putStrLn ("Haskell Modules: " ++ shlist (map mname hms))

showTheoryNames [] = putStrLn "No Required Theories"
showTheoryNames thrys
  = putStrLn ("Required Theories: "++ shlist (map theoryName thrys))

showCurrThry Nothing = putStrLn "No Current Theory"
showCurrThry (Just thry) 
  = putStrLn $ unlines'
      [ "Current Theory: "++theoryName thry
      , " Induction Schemes: "++ (shlist $ map indType $ thIndSchemes thry)
      , " Case-Split Schemes: "++ (shlist $ map caseName $ thCaseSchemes thry)
      ]

showCWD fpath = putStrLn ("Current working directory: "++fpath)

shlist strs = intercalate ", " strs
\end{code}

\newpage
\subsection{Displaying Theory Files}

\begin{code}
showTheoryFiles :: HReqCmdDescr
showTheoryFiles
  = ( "files"
    , "show theory files"
    , "show list of *.thr/*.hs in current directory"
    , showTFiles )

showTFiles _ hreq
  = do listing <- getDirectoryContents "."
       let thrFiles = sort $ filter (isThr `andor` isHS) listing
       putStrLn $ unlines thrFiles
       return hreq

isThr fp = takeExtension fp == ".thr"
isHS fp = takeExtension fp == ".hs"
(f1 `andor` f2) x = f1 x || f2 x

\end{code}

\subsection{Displaying Laws}

\begin{code}
cmdShowLaws :: HReqCmdDescr
cmdShowLaws
  = ( "laws"
    , "'law' names"
    , "show all law and definition names"
    , showLaws )

showLaws :: p -> HReqState -> IO HReqState
showLaws _ hreqs
  = do putStrLn ""
       sequence_ $ map showHModLaws $ hmods hreqs
       putStrLn ""
       sequence_ $ map showTheoryLaws $ hthrys hreqs
       putStrLn ""
       case currThry hreqs of
         Nothing    -> putStrLn "No Current Theory"
         Just thry  -> do showTheoryLaws thry
                          showTheorems thry
       return hreqs

showHModLaws hmod
 = do putStrLn ("Laws in Haskell source '"++mname hmod++"'")
      sequence_ $ map showDecl $ topdecls hmod

showDecl (Fun []) = putStrLn "  !dud function definition!"
showDecl (Fun (m:_))  =  putStrLn ("  " ++ fname m)
showDecl (Bind (Var n) _ _) = putStrLn ("  " ++ n)
showDecl _ = putStrLn "  ??"

showTheoryLaws thry
  = do putStrLn ("Laws in Theory '"++theoryName thry++"'")
       sequence_ $ map showLaw $ thLaws thry

showLaw law = putStrLn ("  "++ lawName law)

showTheorems thry
  = do putStrLn ("Theorems in Theory '"++theoryName thry++"'")
       sequence_ $ map showTheorem $ thTheorems thry
showTheorem thrm = putStrLn ("  "++ thmName thrm)
\end{code}

\subsection{Displaying Theorems}

\begin{code}
cmdShowTheorems :: HReqCmdDescr
cmdShowTheorems
  = ( "thms"
    , "'theorem' names"
    , "show all theorem names"
    , showAllTheorems )

showAllTheorems :: p -> HReqState -> IO HReqState
showAllTheorems _ hreqs
  = do putStrLn ""
       sequence_ $ map showTheorems $ hthrys hreqs
       putStrLn ""
       case currThry hreqs of
         Nothing    -> putStrLn "No Current Theory"
         Just thry  -> showTheorems thry
       return hreqs
\end{code}

\subsection{Load Haskell AST}

\begin{code}
-- deprecated for now
cmdLoadHaskell :: HReqCmdDescr
cmdLoadHaskell
  = ( "lh"
    , "load Haskell source"
    , unlines
        [ "lh <fname>  -- parse and dump AST for <fname>.hs"
        ]
    , loadSource )

loadSource :: [String] -> HReqState -> IO HReqState
loadSource [] hreqs = putStrLn "no file given" >> return hreqs
loadSource (fnroot:_) hreqs
  = do  let fpath = getWD hreqs fnroot
        mdl <- readHaskell fpath
        putStrLn "Module AST:\n"
        let aststr = show mdl
        putStrLn aststr
        writeFile (fpath -<.> "ast") aststr
        -- return $ hmods__ (++[mdl]) hreqs
        return hreqs

readHaskell :: FilePath -> IO Mdl
readHaskell fnroot
  = do let fname = fnroot ++ ".hs"
       modstr <- readFile fname
       parseHModule fname modstr
\end{code}

\newpage
\subsection{Parse Haskell}

\begin{code}
cmdParseHaskell :: HReqCmdDescr
cmdParseHaskell
  = ( "ph"
    , "parse Haskell"
    , "ph <haskell-expr> -- parse haskell expression on command line"
    , parseHaskell )

parseHaskell :: [String] -> HReqState -> IO HReqState
parseHaskell args hreqs
  = do  case hParseE (ParseMode "ph") [] [(1,estr)] of
          But msgs -> putStrLn $ unlines msgs
          Yes ((hsexp,(lno,txt)),_)
            -> do putStrLn "haskell-src parse:"
                  putStrLn $ show hsexp
                  putStrLn (show lno ++ ": first line")
                  putStrLn (show lno ++ " | " ++ txt)
                  let expr = hsExp2Expr preludeFixTab hsexp
                  putStrLn "Simple AST version ::"
                  putStrLn $ show expr
        return hreqs
  where estr = unwords args
\end{code}

\subsection{Load Theory}

\begin{code}
cmdLoadTheory :: HReqCmdDescr
cmdLoadTheory
  = ( "load"
    , "load Theory source"
    , unlines
        [ "load <fngiven>  -- load <fngiven>.thr"
        , "load <fnname>.<ext>  -- load <fname>.<ext>"
        , " -- also loads all haskell modules and theories that it imports"
        , " -- assumes all files are in the same directory"
        ]
    , doLoadTheory )

doLoadTheory :: [String] -> HReqState -> IO HReqState
doLoadTheory [fpath] hreqs 
  = do let (fname,hreqs') = setWD fpath hreqs
       putStrLn ("Working directory is "++wkdir hreqs')
       loadTheory fname hreqs'       
doLoadTheory args hreqs
  = do putStrLn ("Expected one filepath")
       return hreqs

loadTheory :: FilePath -> HReqState -> IO HReqState
loadTheory fname hreqs
  = do let fixname = fixupFileName fname
       let fpath = getWD hreqs fixname 
       res <- readTheory fpath
       case res of
         Nothing -> return hreqs
         Just theory
           -> do putStrLn ("\nLoaded Theory '"++fpath++"'")
                 loadDependencies theory hreqs

fixupFileName :: FilePath -> FilePath
fixupFileName givenFN
  = case ext of
      ""   ->  givenFN  <.> "thr"
      "."  ->  givenFN -<.> "thr"
      _    ->  givenFN 
  where ext = takeExtension givenFN

readTheory :: FilePath -> IO (Maybe Theory)
readTheory fpath
  = do thrystr <- readFile fpath
       case parseTheory (ParseMode fpath) thrystr of
         But msgs  ->  do putStrLn $ unlines msgs
                          return Nothing
         Yes thry  ->  return $ Just thry

cmdReLoadTheory :: HReqCmdDescr
cmdReLoadTheory
  = ( "reload"
    , "reload current theory"
    , unlines
        [ "reload  -- re-load current theory"
        ]
    , doReLoadTheory )

doReLoadTheory :: [String] -> HReqState -> IO HReqState
doReLoadTheory _ hreqs 
  = case currThry hreqs of
      Just thry -> loadTheory (theoryName thry) hreqs
      Nothing   -> do putStrLn "No current theory"
                      return hreqs
\end{code}

\subsection{Load Dependencies}

\begin{code}
loadDependencies :: Theory -> HReqState -> IO HReqState
loadDependencies theory hreqs
  = do hms <- loadModDeps  $ map (getWD hreqs) $ hkImports theory
       ths <- loadThryDeps $ map (getWD hreqs) $ thImports theory
       putStrLn "Theory dependencies loaded.\n"
       return $ currThry_ (Just theory)
              $ hthrys_ ths
              $ hmods_ hms
              $ hreqs

loadModDeps :: [FilePath] -> IO [Mdl]
loadModDeps = sequence . map readHaskell

loadThryDeps :: [FilePath] -> IO [Theory]
loadThryDeps = fmap catMaybes . sequence . map (readTheory . fixupFileName)
\end{code}

\subsection{Check Theorem}


\begin{code}
cmdCheckTheorem :: HReqCmdDescr
cmdCheckTheorem
  = ( "check"
    , "check theorem"
    , "check <name> -- check theorem called name"
    , theoremCheck )

theoremCheck :: [String] -> HReqState -> IO HReqState

theoremCheck [] hreqs
  = do putStrLn "no theorem specified"
       return hreqs

theoremCheck (n:_) hreqs
  = do case currThry hreqs of
         Nothing
           ->  putStrLn "no current theory"
         Just thry
           ->  case findTheorem n $ thTheorems thry of
                 Nothing   ->  putStrLn ("Theorem not found: "++n)
                 Just thm  ->  showReport $
                     checkTheorem 
                       PC { mdls = hmods hreqs
                          , thrys = hthrys hreqs ++ [thry]
                          , supStrat = []
                          }
                       thm
       return hreqs
\end{code}

\newpage
\section{REPL Setup}

\begin{code}
hreqCommands :: HReqCommands
hreqCommands = [ showTheoryFiles
               , cmdLoadTheory
               , cmdReLoadTheory
               , cmdShowState
               , cmdShowLaws
               , cmdShowTheorems
               , cmdCheckTheorem
               -- , cmdLoadHaskell 
               -- , cmdParseHaskell
               ]

hreqConfig
  = REPLC
      hreqPrompt
      hreqEOFreplacmement
      hreqParser
      hreqQuitCmds
      hreqQuit
      hreqHelpCmds
      hreqCommands
      hreqEndCondition
      hreqEndTidy
\end{code}

