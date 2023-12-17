\chapter{Theory}
\input{Copyright}
\begin{code}
module Theory
( Theory(..), parseTheory
, Theorem(..), findTheorem
, Law(..), InductionScheme(..), CaseSplitScheme(..), Case
, Strategy(..), Calculation(..)
, Justification(..), JRel(..), JLaw(..), Usage(..), Focus(..)
)
where

import Prelude hiding(fail)
import Control.Monad.Fail

import Data.Char
import Utilities
import AST
import HParse

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
pdbg nm x = dbg (nm++":\n") x
mdbg msg x = return $! dbg msg x
\end{code}


\section{Theory Document Structure}

\subsection{Starting Off}

Typically a keyword at the start of a line introduces something.
We start with \texttt{THEORY} and zero or more imports:
\def\TOPSYNTAX{\texttt{
\\THEORY <TheoryName>
\\IMPORT-THEORY <FileName>
\\IMPORT-HASKELL <FileName>
}}
\TOPSYNTAX

These are followed by zero or more entries
that describe laws, induction schemes and theorems.

\newpage
\subsection{Laws}

Laws are described by the following ``one-liner'' construct:
\def\LAWSYNTAX{\texttt{
\\LAW <name> <br?> <expr>
}}
\LAWSYNTAX

Here, \verb"<br?>" means that the following part
is either entirely on this line,
or else occupies a number of subsequent lines.
There can be a blank line before it,
and must be a blank line after it.
The following part itself must
not have blank lines embedded in it.

\subsection{Induction Schemes}

An induction-scheme is described by the following four lines:
\def\INDSCHEMASYNTAX{\texttt{
\\INDUCTION-SCHEME <Type>
\\BASE <expr>
\\STEP <var> --> <expr>
\\INJ  <br?> <expr>  ==  <expr>
}}
\INDSCHEMASYNTAX

\subsection{Case-Split Schemes}

A case-split scheme is planned:
\def\CASESCHEMASYNTAX{\texttt{
\\CASE-SCHEME <name>
\\CASE 1 <br?> <expr1> 
\\  ... 
\\CASE N <br?> <exprN> 
\\EXHAUSTIVE <br?> <expr1> || ... || <exprN>
\\EXCLUSIVE 1 2 <br?> not(<expr1> \&\& <expr2>)
\\ -- use order  1 3 ; 1 4 ; ... ; N-2 N-1 ; N-2 N ; N-1 N
\\EXCLUSIVE (N-1) N <br?> not(<exprN-1> \&\& <exprN>)
}}
\CASESCHEMASYNTAX

\subsection{Theorems}

A theorem has the following top-level structure:
\def\THEOREMSYNTAX{\texttt{
\\THEOREM <name>  <br?> <expr>
\\STRATEGY <strategy-type>
\\  <strategy-body>
\\END <strategy-type>
\\QED <name>
}}
\THEOREMSYNTAX

\subsection{Strategy Types}

Strategy types include:
\def\ReduceAll{ReduceAll}
\def\ReduceLHS{ReduceLHS}
\def\ReduceRHS{ReduceRHS}
\def\ReduceBoth{ReduceBoth}
\def\CaseSplit{CaseSplit}
\def\STRATEGIES{\texttt{
\\\ReduceAll
\\\ReduceLHS
\\\ReduceRHS
\\\ReduceBoth
\\\CaseSplit\ <case-name>
}}
\def\Induction{Induction}
\def\DOINDUCTION{\texttt{
\\\Induction\ <ind-var> :: <type>
}}
\def\SDOINDUCTION{\texttt{
\\STRATEGY Induction <ind-var> :: <type>
}}

\STRATEGIES
\DOINDUCTION

\newpage
\subsection{Strategy Bodies}

The choice of strategy will then determine the resulting structure:
\def\INDUCTIONSYNTAX{\texttt{
\\BASE <val> <br!> <expr>
\\<one of the other four strategies>
\\END BASE
\\STEP <expr>
\\ASSUME <br!> <expr>
\\SHOW <br!> <expr>
\\<one of the other four strategies>
\\END STEP
}}
\def\CALCULATION{\texttt{
<calculation>
}}
\def\REDBOTHSYNTAX{\texttt{
\\LHS
\\<calculation>
\\RHS
\\<calculation>
}}

\subsubsection{ReduceAll}

\CALCULATION

\subsubsection{ReduceLHS}

\CALCULATION

\subsubsection{ReduceRHS}
\CALCULATION

\subsubsection{ReduceBoth}
   \REDBOTHSYNTAX


\subsubsection{CaseSplit}
\def\CASES{\texttt{
\\ CASE 1 <br!>  <expr1> )
\\ <case-body>
\\ END CASE 1
\\...
\\ CASE N <br!> <exprN>
\\ <case-body>
\\ END CASE n
}}
\CASES
\\Here, \verb"<br!>" is similar to \verb"<br?>",
    except that a line break at this point is mandatory.

\subsubsection{CaseBody}

\def\CASEBODY#1{\texttt{
\\ SHOW <br?> <expr{#1}>
\\ STRATEGY <strategy-type>
\\ <strategy-body>
\\ END <strategy-type>
}}
\CASEBODY{C}
\\ Here, \texttt{C} is the number of the relevant case

\subsubsection{Induction}
    \INDUCTIONSYNTAX

\subsection{Calculations}

A calculation is a sequence of expressions seperated by justification lines,
which always start with an equal sign. Blank lines are allowed
around justification lines.
\def\CALCSYNTAX{\texttt{
\\<expr1>
\\ = <justification1>
\\ ...
\\ = <justificationN>
\\<exprN+1>
}}
\CALCSYNTAX

\subsection{Justifications}

The justification format is as follows:
\lstinputlisting[basicstyle=\ttfamily]{doc/justifications.txt}


\newpage
\section{Theory Datatypes}

\subsection{Top-Level Datatype}

\TOPSYNTAX \dots
\begin{code}
data Theory
 = THEORY {
     theoryName    :: String
   , thImports     :: [String]  -- Theory Names
   , hkImports     :: [String]  -- Haskell Module names
   , thLaws        :: [Law]
   , thIndSchemes  :: [InductionScheme]
   , thCaseSchemes :: [CaseSplitScheme]
   , thTheorems    :: [Theorem]
   }
 deriving Show

thImports__     f thry = thry{ thImports     = f $ thImports     thry }
hkImports__     f thry = thry{ hkImports     = f $ hkImports     thry }
thLaws__        f thry = thry{ thLaws        = f $ thLaws        thry }
thIndSchemes__  f thry = thry{ thIndSchemes  = f $ thIndSchemes  thry  }
thCaseSchemes__ f thry = thry{ thCaseSchemes = f $ thCaseSchemes thry }
thCaseSchemes_  x = thCaseSchemes__ $ const x
thTheorems__    f thry = thry{ thTheorems    = f $ thTheorems    thry }
\end{code}



\subsection{Law Datatype}

\LAWSYNTAX
\begin{code}
data Law
 = LAW {
     lawName :: String
   , lawEqn :: Expr
   , lawLine :: Line
   }
 deriving Show
\end{code}

\subsection{Induction Scheme Datatype}

\INDSCHEMASYNTAX
\begin{code}
data InductionScheme
 = IND {
     indType :: String
   , indVar  :: String  -- generic induction variable
   , indBase :: Expr    -- base value
   , indStep :: Expr    -- induction var to step expression
   }
 deriving Show
\end{code}

\subsection{Case-Split Scheme Datatype}

\CASESCHEMASYNTAX
\begin{code}
data CaseSplitScheme
 = CASE {
     caseName :: String
   , cases :: [Expr]    -- at least two cases
   , caseLines :: Lines -- name, then each expr
   }
 deriving Show
\end{code}

\subsection{Theorem Datatype}

\THEOREMSYNTAX
\begin{code}
data Theorem
 = THEOREM {
     thmName :: String
   , theorem :: Expr
   , strategy :: Strategy
   , thmLine :: Line -- for theorem
   }
 deriving Show
\end{code}

\newpage
\subsection{Strategy Datatype}

\STRATEGIES
\begin{code}
data Strategy
 = ReduceAll Calculation
 | ReduceLHS Calculation
 | ReduceRHS Calculation
 | ReduceBoth Calculation Calculation
\end{code}
\SDOINDUCTION
\INDUCTIONSYNTAX
\begin{code}
 | Induction { -- goal is what we are proving by induction
     iVar :: (String,String)  -- var :: type
   , iVarLine :: Line
   , baseVal :: Expr          -- base value
   , bValLine :: Line
   , bGoal :: Expr            --  goal[baseVal/var]
   , bGoalLine :: Line
   , baseStrategy :: Strategy
   , stepExpr :: Expr         -- expr
   , stepLine :: Line
   , assume :: Expr           -- goal
   , assLine :: Line
   , iGoal :: Expr            -- goal[stepExpr/var]
   , iGoalLine :: Line
   , stepStrategy :: Strategy
   }
\end{code}
\CASES
\\where \texttt{<case-body>} is
\CASEBODY{C}
\begin{code}
 | CaseSplit { 
     csName :: String
   , csCases :: [Case]
   , caseSLines :: Line  -- name 
   }
\end{code}
\begin{code}
 deriving Show

type Case = (Int,Expr,Strategy,Line)
\end{code}



\newpage
\subsection{Calculation Datatype}

\CALCSYNTAX
\begin{code}
data Calculation
 = CALC {
     goal :: Expr
   , calcs :: [(Justification,Expr,Line)] -- line of expression
   , goalLine :: Line
   }
 deriving Show
\end{code}

\subsection{Justification Datatype}

Justifications:
\lstinputlisting[basicstyle=\ttfamily]{doc/justifications.txt}
\begin{code}
data Justification
 = BECAUSE {
     jrel :: JRel
   , law :: JLaw
   , usage :: Usage
   , focus :: Focus
   , jLine :: Line
   }
 deriving Show
data JRel = JEq deriving (Eq, Show)
data JLaw 
  = L String      -- law name
  | D String Int  -- defined name and clause number {1,..,N}
  | IF Int        -- if expression branch number  {1,2}
  | GRD Int       -- guarded-if branch number  {1,..,N}
  | IH            -- inductive hypothesis (as assumption)
  | CA String Int -- case assumption with scheme name and case number
  | SMP           -- built-in simplifier
  | NORM String   -- normalises nested associative+commutative operators
  deriving (Eq, Show)
data Usage = Whole | L2R | R2L deriving (Eq, Show)
data Focus = Top | At String Int deriving Eq

instance Show Focus where
  show Top       =  "top-level"
  show (At s i)  =  showth i ++ " " ++ s

showth 1 = "1st"
showth 2 = "2nd"
showth 3 = "3rd"
showth n = show n ++ "th" -- valid up to 20th...
\end{code}

\newpage
\section{Parser Top-Level}



We start by adding in an ``empty'' theory as an accumulating
parameter,
breaking input into numbered lines
and starting the proper parsing.
\begin{code}
parseTheory :: (MonadFail m) => ParseMode -> String -> m Theory
parseTheory pmode str
  = do (thry,_) <- theoryParser pmode theory0 $ zip [1..] $ lines str
       return thry

theory0 
  = THEORY { 
      theoryName    = "?"
    , thImports     = []
    , hkImports     = []
    , thLaws        = []
    , thIndSchemes  = []
    , thCaseSchemes = []
    , thTheorems    = [] 
    }
\end{code}
We start proper parsing by looking for \texttt{THEORY <TheoryName>}
on the first line:
\begin{code}
theoryParser :: (MonadFail m) => ParseMode -> Theory -> Parser m Theory
theoryParser pmode theory lns
 = do ((thryNm,_),lns') <- requireKeyAndName "THEORY" lns
      parseBody pmode theory{theoryName = thryNm} lns'
\end{code}

Once \texttt{THEORY} is found, work through the body of the text:
\begin{code}
parseBody :: (MonadFail m) => ParseMode -> Theory -> Parser m Theory
parseBody pmode theory [] = return (theory, [])
parseBody pmode theory (ln@(lno,str):lns)
 -- we skip empty lines here...
 | emptyLine str  =  parseBody pmode theory lns

 -- simple one-liners
 | gotImpTheory   =  parseBody pmode (thImports__ (++[thryName]) theory) lns
 | gotImpCode     =  parseBody pmode (hkImports__ (++[codeName]) theory) lns

 -- complex parsers
 | gotIndSchema  = parse(parseIndSchema pmode theory typeName lno)     lns
 | gotCaseSchema = parse(parseCaseSchema pmode theory casesName)       lns
 | gotLaw        = parse(parseLaw pmode theory lwName lno lrest)       lns
 | gotTheorem    = parse(parseTheorem pmode theory thrmName lno trest) lns

 | otherwise     
   = pFail pmode lno 1 
       (unlines
         [ "expecting IMPORT-X, LAW,"
         , "          INDUCTION-SCHEME, CASE-SCHEME, THEOREM" ])
      str
 where
   (gotImpTheory, thryName)       =  parseKeyAndName "IMPORT-THEORY"    str
   (gotImpCode,   codeName)       =  parseKeyAndName "IMPORT-HASKELL"   str
   (gotLaw, lwName, lrest)        =  parseOneLinerStart "LAW"           str
   (gotIndSchema, typeName)       =  parseKeyAndName "INDUCTION-SCHEME" str
   (gotCaseSchema, casesName)     =  parseKeyAndName "CASE-SCHEME"      str
   (gotTheorem, thrmName, trest)  =  parseOneLinerStart "THEOREM"       str

   parse parser lns
     = do (theory',lns') <- parser lns
          parseBody pmode theory' lns'
\end{code}

\newpage
\section{Parse Laws}

\LAWSYNTAX
\begin{code}
parseLaw :: (MonadFail m) => ParseMode -> Theory  -> String -> Int -> String
         -> Parser m Theory
parseLaw pmode theory lwName lno rest lns
  = case parseExprChunk pmode lno rest lns of
      But msgs
        ->  pFail pmode lno 1 (unlines msgs) $ headstr lns
      Yes ((expr,eln), lns')
        ->  return (thLaws__ (++[LAW lwName expr eln]) theory, lns')

parseExprChunk :: (MonadFail m) => ParseMode -> Int -> String 
               -> Parser m (Expr,Line)
parseExprChunk pmode lno rest lns
 | emptyLine rest  =  parseExpr pmode restlns chunk
 | otherwise       =  parseExpr pmode lns     [(lno,rest)]
 where (chunk,restlns) = getChunk lns
\end{code}

\section{Parse Induction Schemata}

\INDSCHEMASYNTAX
\begin{code}
parseIndSchema :: (MonadFail m) => ParseMode -> Theory -> String -> Int
               -> Parser m Theory
parseIndSchema pmode theory typeName lno (ln1:ln2:ln3:lns)
 | not gotBase  
    =  pFail pmode (lno+1) 1 "INDUCTION-SCHEME: missing BASE" (snd ln1)
 | not gotStep  
    =  pFail pmode (lno+2) 1 "INDUCTION-SCHEME: missing STEP" (snd ln2)
 | not gotInj   
    =  pFail pmode (lno+3) 1 "INDUCTION-SCHEME: missing INJ"  (snd ln3)
 | otherwise
     =  case parseEquivChunk pmode (lno+3) ln3rest lns of
         Nothing
           ->  pFail pmode lno 1 "INJ: Injective law expected" (snd ln3)
         Just ((e1,e2), lns')
           ->  parseBody pmode (thIndSchemes__ (ind:) theory) lns'
 where
   (gotBase,bValue) = parseKeyAndValue pmode "BASE" $ snd ln1
   (gotStep,sVar,eStep) = parseKeyNameKeyValue pmode "STEP" "-->" $ snd ln2
   len = length "INJ"
   (ln3inj,ln3rest) = splitAt len $ snd ln3
   gotInj = ln3inj == "INJ"
   ind = IND typeName sVar bValue eStep
parseIndSchema pmode theory typeName lno lns
  = pFail pmode lno 0 "INDUCTION-SCHEME: Incomplete" $ headstr lns
\end{code}

Look for two expressions connected by `equality'.
\begin{code}
parseEquivChunk :: (MonadFail m) => ParseMode -> Int -> String
                -> Parser m (Expr,Expr)
parseEquivChunk pmode lno rest lns
 | emptyLine rest  =  parseEqual pmode restlns chunk
 | otherwise       =  parseEqual pmode lns     [(lno,rest)]
 where (chunk,restlns) = getChunk lns
\end{code}

\newpage
\section{Parse Cases Schemata}
\CASESCHEMASYNTAX
\begin{code}
parseCaseSchema :: (MonadFail m) => ParseMode -> Theory -> String
                -> Parser m Theory
-- seen CASE-SCHEME <name>
-- expecting CASE I <br?> <exprI>  for I in 1..
parseCaseSchema pmode theory casesName lns
  =  case parseCases pmode 0 [] lns of
       But msgs -> pFail pmode 99991 0  
                     ("CASE-SCHEME "++casesName++":\n"++unlines' msgs)
                     str
       Yes (cslns,lns') 
         ->  do let (cases,clns) = unzip cslns
                let csScheme = CASE casesName cases (ln:clns)
                let theory' = thCaseSchemes__ (csScheme:) theory
                parseExhaustive pmode theory' cases lns'
  where ln@(_,str) = headln lns


parseCases :: MonadFail m => ParseMode 
           -> Int    -- number of CASEs seen so far
           -> [(Expr,Line)] -- Expr collected so far, in reverse order
           -> Parser m [(Expr,Line)]
parseCases pmode _ _ []  
  =  pFail pmode 99992 0 "CASE-SCHEME: Incomplete" ""
parseCases pmode ccount srpxe  ll@((lno,str):lns)
  = case words str of
      ("EXHAUSTIVE":_) -- we don't parse this right now
          -> return (reverse srpxe,ll)
      ("CASE":num:wrest)
        | all isDigit num && read num == nextc
          ->  case parseExpr pmode [] [(lno,unwords wrest)] of
                Yes (el@(expr,eln),_) 
                  -> parseCases pmode nextc (el:srpxe) lns
                But msgs -> pFail pmode lno 0 (unlines' msgs) str
      _ -> pFail pmode lno 0 "Expected CASE or EXHAUSTIVE" str
  where
    nextc = ccount+1

parseExhaustive :: (MonadFail m) => ParseMode -> Theory -> [Expr]
                -> Parser m Theory
parseExhaustive pmode theory cases ((lno,str):lns)
  = case words str of
      ("EXHAUSTIVE":_) -- we don't parse this right now
          -> parseExclusives pmode theory cases lns
      _ -> pFail pmode lno 0 ("CASE-SCHEME: expected EXHAUSTIVE ") str

parseExclusives pmode theory cases lns
  = let
      ncases = zip [1..] cases
      casepairs = pairup ncases
    in parseExclusive pmode theory casepairs lns

parseExclusive pmode theory [] lns
  = parseBody pmode theory lns
parseExclusive pmode theory (cp:cps) []
  = pFail pmode 99993 0 
     "Expecting EXAHUSTIVE: Premature end of file" ""
parseExclusive pmode theory (((m,e1),(n,e2)):cps) ((lno,str):lns)
  = case words str of
      ("EXCLUSIVE":num1:num2:wrest)
        | all isDigit num1 && all isDigit num2
          -> if read num1 /= m || read num2 /= n
             then pFail pmode lno 0 
                   ("Expected EXCLUSIVE "++show m++" "++show n) str
             else parseExclusive pmode theory cps lns
             -- we don't check expressions here
\end{code}

\newpage
\section{Parse Theorems}

\THEOREMSYNTAX
\begin{code}
-- seen "THEOREM <name>"
parseTheorem :: (MonadFail m) => ParseMode -> Theory -> String -> Int -> String
             -> Parser m Theory
parseTheorem pmode theory thrmName lno rest lns
  = case parseExprChunk pmode lno rest lns of
      Nothing
        ->  pFail pmode lno 0 "Theorem expression expected" str
      Just ((goal,gln), lns)
        -> do (strat,lns) <- parseStrategy pmode lns
              (qedNm,lns) <- requireKeyAndName "QED" lns
              -- should check qedNm = thrmName
              let thry = THEOREM thrmName goal strat gln
              let theory' = thTheorems__ (++[thry]) theory
              return (theory',lns)
  where ln@(_,str) = headln lns
\end{code}

\subsection{Parse Reduction Strategies}

\begin{code}
-- seen strategy goal expression
parseStrategy :: (MonadFail m) => ParseMode -> Parser m Strategy
parseStrategy pmode [] 
  = pFail pmode maxBound 0 "STRATEGY: premature end of file" ""
parseStrategy pmode (ln@(lno,str):lns)
  | gotReduce     =  parseReduction pmode rstrat lns
  | gotInduction  =  parseInduction pmode ((var,typ),vtln) lns
  | gotCaseSplit  =  parseCaseSplit pmode csname 0 [] lns
  | otherwise     =  pFail pmode lno 0 
                       "STRATEGY <strategy-type> expected." str
  where
    (gotReduce,rstrat) = parseRedStratDecl ln 
    (gotInduction,(var,typ,vtln)) = parseIndStratDecl ln 
    (gotCaseSplit,csname) = parseCaseSplitStratDecl ln 
\end{code}

\STRATEGIES
\begin{code}
parseRedStratDecl ln@(_,str)
  | stratSpec == ["STRATEGY","ReduceAll"] = (True,ReduceAll udefc)
  | stratSpec == ["STRATEGY","ReduceLHS"] =  (True,ReduceLHS udefc)
  | stratSpec == ["STRATEGY","ReduceRHS"] =  (True,ReduceRHS  udefc)
  | stratSpec == ["STRATEGY","ReduceBoth"]=  (True,ReduceBoth udefc udefc)
  | otherwise  =  (False,ReduceAll udefc)
  where
    stratSpec = words str
    udefc = CALC (Var "undefined reduce calculation") [] ln
\end{code}

\newpage
Based on the reduction strategy,
we determine a pattern of calls to \texttt{parseReduction'} 
that does the work.
\begin{code}
parseReduction :: (MonadFail m) => ParseMode -> Strategy
               -> Parser m Strategy
\end{code}
\texttt{
\\\ReduceAll | \ReduceLHS | \ReduceRHS
\\<calculation>
}
\begin{code}
-- single reductions all end with "END"
parseReduction pm (ReduceAll _) lns  =  parseReduction' pm "END" ReduceAll lns
parseReduction pm (ReduceLHS _) lns  =  parseReduction' pm "END" ReduceLHS lns
parseReduction pm (ReduceRHS _) lns  =  parseReduction' pm "END" ReduceRHS lns
\end{code}
\texttt{
\\\ReduceBoth
\REDBOTHSYNTAX
}
\begin{code}
parseReduction pm (ReduceBoth _ _) lns
 = do (_,lns) <- requireKey "LHS" lns
      -- first reduction ends with "RHS"
      (ReduceAll red1,lns) <- parseReduction' pm "RHS" ReduceAll lns
      -- second reduction ends as for other reduce strategies
      (ReduceAll red2,lns) <- parseReduction' pm "END" ReduceAll lns
      return (ReduceBoth red1 red2,lns)
\end{code}

This is were the parsing is done.
\begin{code}
parseReduction' :: (MonadFail m) => ParseMode -> String 
                -> (Calculation -> Strategy)
                -> Parser m Strategy
parseReduction' pmode calcStop reduce lns
 = do (calc, lns') <- parseCalculation pmode calcStop lns
      completeCalc pmode calcStop reduce calc lns'

completeCalc :: (MonadFail m) => ParseMode -> String
             -> (Calculation -> Strategy) -> Calculation
             -> Parser m Strategy
completeCalc pmode calcStop _ _ []
  = pFail pmode 0 0 
       ( unlines
           [ "Premature end of file"
           , "Expecting proof steps, or "++ calcStop++" <name>" ] )
       ""
completeCalc pmode calcStop reduce calc ((num,str):lns)
 | head (words str) == calcStop  =  return (reduce calc,lns)
 | otherwise  =  pFail pmode num 0 
                   ("Improper calc end, expecting: "++ calcStop)
                   str
\end{code}

\newpage
\subsection{Parse Induction Strategies}

\SDOINDUCTION
\begin{code}
parseIndStratDecl ln@(_,str)
  = case words str of
      ("STRATEGY":"Induction":indtvars)  ->  parseIndVars ln indtvars
      _ -> (False,("not an induction strategy","",ln))

parseIndVars ln [] = (False,("no induction variables defined.","",ln))
parseIndVars ln [var,"::",typ] = (True, (var,typ,ln))
parseIndVars ln _ = (False, ("Expected var :: type","",ln))
\end{code}

\INDUCTIONSYNTAX
\begin{code}
-- seen "STRATEGY Induction ...."
parseInduction :: MonadFail m => ParseMode -> ((String,String),Line) 
               -> Parser m Strategy
parseInduction pmode _ []
  = pFail pmode 99994 0 "Induction proof: premature end-of-file" ""
parseInduction pmode (vartyp,vtln) lns
  = do ((bval,bvln),lns) <- requireKeyAndValue pmode "BASE" lns
       ((bexpr,beln),lns) <- parseExprChunk pmode 0 [] lns
       (bstrat,lns) <- parseStrategy pmode lns
       (endB,lns) <- requireKeyAndName "END" lns 
       ((sexpr,seln),lns) <- requireKeyAndValue pmode "STEP" lns
       (_,lns) <- requireKey "ASSUME" lns
       ((ass,aln),lns) <- parseExprChunk pmode 0 [] lns -- FIX
       (_,lns) <- requireKey "SHOW" lns
       ((goal,gln),lns) <- parseExprChunk pmode 0 [] lns -- FIX
       (sstrat,lns) <- parseStrategy pmode lns
       (endS,lns) <- requireKeyAndName "END" lns 
       (endNm,lns) <- requireKeyAndName "END" lns 
       -- should check endNm against 'Induction''
       return ( Induction { iVar = vartyp
                          , iVarLine = vtln
                          , baseVal = bval
                          , bValLine = bvln
                          , bGoal = bexpr
                          , bGoalLine = beln
                          , baseStrategy = bstrat
                          , stepExpr = sexpr
                          , stepLine = seln
                          , assume = ass
                          , assLine = aln
                          , iGoal = goal
                          , iGoalLine = gln
                          , stepStrategy = sstrat
                          }
              , lns
              )
  where ln = headln lns
\end{code}

\newpage
\subsection{Parse Case-Split Strategies}
\CASES
\\where \texttt{<case-body>} is
\CASEBODY{C}
\begin{code}
parseCaseSplitStratDecl ln@(_,str)
  = case words str of
      (["STRATEGY","CaseSplit",nm])  ->  (True,nm)
      _   ->   (False,"not a case-split strategy")

-- seen "STRATEGY CaseSplit name"
parseCaseSplit :: MonadFail m => ParseMode -> String -> Int -> [Case]
               -> Parser m Strategy
parseCaseSplit pmode _ _ _ []
  = pFail pmode 99995 0 "Case-split proof: premature end-of-file" ""
parseCaseSplit pmode caseNm count sesac ((lno,str):lns)
  = case words str of
      [] -> parseCaseSplit pmode caseNm count sesac lns
      (["END","CaseSplit"]) 
        -> if null sesac
           then pFail pmode lno 0 "CaseSplit: no cases!" str
           else do let cases@((_,_,_,cln):_) = reverse sesac
                   return ( CaseSplit { csName = caseNm
                                      , csCases = cases
                                     , caseSLines = cln }
                          , lns)
      ("CASE":num:wrest)
        | all isDigit num && read num == ncount
          -> do ((cexpr,cln),_) <- parseExpr pmode [] [(lno,unwords wrest)]
                (_,lns) <- requireKey "SHOW" lns
                (shexpr,lns) <- parseExprChunk pmode 0 [] lns
                (cstrat,lns) <- parseStrategy pmode lns
                (cntexpr,lns) <- require2KeysAndValue pmode "END" "CASE" lns
                -- should check cntexpr = ncount
                let thisCase = (ncount,cexpr,cstrat,cln)
                parseCaseSplit pmode caseNm ncount (thisCase:sesac) lns

      wrds -> pFail pmode lno 0 
                ( "CaseSplit, expected 'QED " ++ caseNm
                  ++ "' or 'CASE " ++show ncount ++ "' <expr>")
                str
  where ncount = count+1
\end{code}

\newpage
\subsection{Parse Calculations}


\CALCSYNTAX
\begin{code}
type Steps = [(Line,Lines)]
\end{code}

This requires multiple ``chunks'' to be parsed.
Blank lines are separators,
as are lines beginning with a leading space followed by a single equal sign.
A calculation is ended by 
a line starting with the word \texttt{calcStop}.
\begin{code}
parseCalculation :: (MonadFail m) 
                 => ParseMode -> String -> Parser m Calculation
parseCalculation pmode calcStop lns
  = do (calcChunks,rest) <- takeLinesBefore calcStop lns
       ((fstChunk,sepChunks),_) 
                    <- splitLinesOn pmode isJustificationLn calcChunks
       ((goalPred,gln),lns') <- parseExpr pmode [] fstChunk
       steps <- parseSteps pmode sepChunks
       return (CALC goalPred steps gln, rest)
\end{code}

Break line-list at the first use of a designated keyword,
discarding empty lines along the way
\begin{code}
takeLinesBefore :: (MonadFail m) => String -> Parser m Lines
takeLinesBefore _ []    =  return ( [], [] )
takeLinesBefore key lns@(ln:lns')
 | null lnwords         =  takeLinesBefore key lns'
 | head lnwords == key  =  return ( [], lns )
 | otherwise                  
   =  do (before,after) <- takeLinesBefore key lns'
         return ( ln:before, after )
 where lnwords = words $ snd ln
\end{code}

A justification line has a first word that is an equals-sign (for now).
\begin{code}
isJustificationLn :: Line -> Bool
isJustificationLn (_,str)  =  case words str of
                                []     ->  False
                                (w:_)  ->  w `elem` ["="]
\end{code}

\newpage

Split into maximal chunks seperated by lines that satisfy \texttt{splitHere}:
\begin{code}
splitLinesOn :: (MonadFail m)
             => ParseMode -> (Line -> Bool) -> Parser m (Lines,Steps)

-- we expect at least one line before split
splitLinesOn pmode splitHere [] 
  = pFail pmode 0 0 "splitLinesOn: premature end of calc." ""
splitLinesOn pmode splitHere (ln:lns)
 | splitHere ln  
   = pFail pmode (fst ln) 0 "Expecting expression" (snd ln)
 | otherwise  
   =  splitLinesOn' pmode splitHere [ln] lns

-- seen initial chunk, looking for first split
splitLinesOn' pmode splitHere knuhc []  =  return ((reverse knuhc,[]),[])
splitLinesOn' pmode splitHere knuhc (ln:lns)
 | splitHere ln  
   =  splitLinesOn'' pmode splitHere (reverse knuhc) [] ln [] lns
 | otherwise  
   = splitLinesOn' pmode splitHere (ln:knuhc) lns

-- found split
-- accumulating post-split chunk
splitLinesOn'' pmode splitHere chunk0 spets split knuhc []
 | null knuhc  
   =  pFail pmode (fst split) 0 "splitLinesOn'':premature end of calc." ""
 | otherwise  
   =  return ( ( chunk0
               , reverse ((split, reverse knuhc):spets) )
               , [] )
splitLinesOn'' pmode splitHere chunk0 spets split knuhc (ln:lns)
 | splitHere ln  
   =  splitLinesOn'' pmode splitHere chunk0 
         ((split, reverse knuhc):spets) ln [] lns
 | otherwise  
   =  splitLinesOn'' pmode splitHere chunk0 spets split (ln:knuhc) lns
\end{code}

Parsing calculation steps:
\begin{code}
parseSteps :: (MonadFail m) => ParseMode -> Steps 
           -> m [(Justification,Expr,Line)]
parseSteps pmode [] = return []
parseSteps pmode ((justify,chunk):rest)
  = do just <- parseJustification pmode justify
       ((exp,eln),_) <- parseExpr pmode [] chunk
       steps <- parseSteps pmode rest
       return ((just,exp,eln):steps)
\end{code}

\newpage
Parsing a justification.

\lstinputlisting[basicstyle=\ttfamily]{doc/justifications.txt}

Parsing of whole line --- need at least two words
\begin{code}
parseJustification :: (MonadFail m) => ParseMode -> Line -> m Justification
parseJustification pmode (lno,str)
 = case words str of
    (w1:w2:wrest) ->  do jr <- parseJRel w1
                         parseJustify pmode lno str jr wrest w2
    _ ->  pFail pmode lno 0 "incomplete justification" str
 where
    parseJRel "="  =  return JEq
    parseJRel  x   
      =  pFail pmode lno 1 ("unrecognised proof relation"++x) str
\end{code}

Parsing given at least two words, the first of which is OK.
If we get a succesful parse, we ignore anything leftover.
\begin{code}
parseJustify :: (MonadFail m) 
             => ParseMode 
             -> Int -> String -> JRel -> [String] -> String
             -> m Justification
parseJustify pmode lno str jr wrest w2
 | w2 == "LAW"    = parseLawName pmode lno str jr     wrest
 | w2 == "DEF"    = parseDef     pmode lno str jr     wrest
 | w2 == "IF"     = parseIf      pmode lno str jr     wrest
 | w2 == "GRDIF"  = parseGuard   pmode lno str jr     wrest
 | w2 == "INDHYP" = parseUsage   pmode lno str jr IH  wrest
 | w2 == "CASEP"  = parseCaseP   pmode lno str jr     wrest
 | w2 == "SIMP"   = parseUsage   pmode lno str jr SMP wrest
 | w2 == "NORM"   = parseNorm    pmode lno str jr     wrest
 | otherwise      
     = pFail pmode lno 1
          ("unrecognised law specification: "++w2)
          str
\end{code}


Seen a \texttt{LAW}, expecting a \texttt{fname}
\begin{code}
parseLawName pmode lno str jr []         
  =  pFail pmode lno 0 "LAW missing name" str
parseLawName pmode lno str jr (w:wrest)  
  =  parseUsage pmode lno str jr (L w) wrest
\end{code}

\newpage
Seen a \texttt{DEF}, expecting a \texttt{fname[.i]}
\begin{code}
parseDef pmode lno str jr [] 
 = pFail pmode lno 0 "DEF missing name" str
parseDef pmode lno str jr (w:wrest) 
  =  parseUsage pmode lno str jr (mkD w) wrest

mkD w -- any error in '.loc' results in value 0
  | nm == w          =  D nm 1
  | null dotloc      =  D w 0
  | null loc         =  D nm 0
  | all isDigit loc  =  D nm $ read loc
  | otherwise        =  D nm 0
  where
    (nm,dotloc) = break (=='.') w
    loc = tail dotloc
\end{code}

Seen a \texttt{IF}, expecting a number.
\begin{code}
parseIf pmode lno str jr (n:wrest)  
  | all isDigit n       
      =  parseUsage pmode lno str jr (IF $ read n) wrest
parseIf pmode lno str jr _  
      =  pFail pmode lno 0 "IF missing number" str
\end{code}

Seen a \texttt{GRDIF}, expecting a number.
\begin{code}
parseGuard pmode lno str jr (n:wrest)
  | all isDigit n          
     =  parseUsage pmode lno str jr (GRD $ read n) wrest
parseGuard pmode lno str jr _  
  =  pFail pmode lno 0 "GRDIF missing number" str
\end{code}

Seen a \texttt{CASEP}, expecting a case-scheme name and a case number.
\begin{code}
parseCaseP pmode lno str jr (w:n:wr)
  | all isDigit n  =  parseUsage pmode lno str jr (CA w $ read n) wr
parseCaseP pmode lno str jr _  
  =  pFail pmode lno 0 "CASEP missing name and/or number" str
\end{code}


parseCaseP   pmode lno str jr     wrest

Seen law, looking for optional usage.
\begin{code}
parseUsage pmode lno str jr jlaw []
  =  return $ BECAUSE jr jlaw (defUsage jlaw) (defFocus jlaw) (lno,str)
parseUsage pmode lno str jr jlaw ws@(w:wrest)
  | w == "l2r"  =  parseFocus pmode lno str jr jlaw L2R wrest
  | w == "r2l"  =  parseFocus pmode lno str jr jlaw R2L wrest
  | otherwise   =  parseFocus pmode lno str jr jlaw (defUsage jlaw) ws

defUsage D   {}  =  L2R
defUsage IF  {}  =  L2R  
defUsage GRD {}  =  L2R  
defUsage _       =  Whole
\end{code}

Seen law and possible usage, looking for optional focus.
Expecting either \texttt{@ name} or \texttt{@ name i}
\begin{code}
parseFocus pmode lno str jr jlaw u []
  =  return $ BECAUSE jr jlaw u (defFocus jlaw) (lno,str)
parseFocus pmode lno str jr jlaw u [w1,w2]
  | w1 == "@"  =  return $ BECAUSE jr jlaw u (At w2 1) (lno,str)
parseFocus pmode lno str jr jlaw u [w1,w2,w3]
  | w1 == "@" && all isDigit w3  
               =  return $ BECAUSE jr jlaw u (At w2 (read w3)) (lno,str)
parseFocus pmode lno str jr jlaw u ws
     =  pFail pmode lno 0 ("invalid focus: "++unwords ws) str

defFocus (D n _)  =  At n 1
defFocus GRD {}   =  At "guarded-if" 1
defFocus IF {}    =  At "if" 1
defFocus _        =  Top
\end{code}


Seen a \texttt{NROM}, expecting name of associative+commutative operators
\begin{code}
parseNorm pmode lno str jr (op:wrest)  
  | op `elem` ["+","*","||","&&"]
  =  parseUsage pmode lno str jr (NORM op) wrest
parseNorm pmode lno str jr _  
      =  pFail pmode lno 0 "NORM missing allowed operator" str
\end{code}



\newpage
\section{``One-Liner'' Parsing}

\subsection{Speculative line-parses}

The following line parsers check to see if a line has a particular form,
returning a true boolean value that is so,
plus extra information if required.


\begin{code}
emptyLine :: String -> Bool
emptyLine str = all isSpace str || take 2 (dropWhile isSpace str) == "--"
\end{code}

We return a boolean that is true if the parse suceeds.
\begin{code}
parseKeyAndName :: String -> String -> (Bool, String)
parseKeyAndName key str
  = case words str of
      [w1,w2] | w1 == key  ->  (True,  w2)
      _                    ->  (False, ("Expecting '"++key++"' and name"))
\end{code}

\begin{code}
parseKeyAndValue :: ParseMode -> String -> String -> (Bool, Expr)
parseKeyAndValue pmode key str
  = case words str of
      (w1:wrest) | w1 == key
         -> case parseExpr pmode [] [(0,unwords wrest)] of
            Nothing            
              ->  (False, Var ("Bad value: "++ unwords wrest))
            Just ((hsexp,_),_) 
              ->  (True,  hsexp)
      _  ->  (False, Var ("Expecting '"++key++"' and value"))
\end{code}

\begin{code}
parseKeyNameKeyValue :: ParseMode -> String -> String -> String
                     -> (Bool,String,Expr)
parseKeyNameKeyValue pmode key1 key2 str
  = case words str of
      (w1:w2:w3:wrest) | w1 == key1 && w3 == key2
        ->  case parseExpr pmode [] [(0,unwords wrest)] of
              Nothing  ->  (False, "", Var ("Bad value: "++ unwords wrest))
              Just ((hsexp,_),_) 
                       ->  (True,  w2, hsexp)
      _ ->    (False, "", Var ("Expecting '"++key2++"' and value"))
\end{code}

\begin{code}
parseOneLinerStart :: String -> String -> (Bool,String,String)
parseOneLinerStart key str
  = case words str of
      (w1:w2:rest) | w1 == key  ->  (True,  w2, unwords rest)
      _                         ->  ( False
                                    , "parseOneLinerStart failed!"
                                    , str)
\end{code}

\newpage
\subsection{Mandatory one-liners}

These parsers expect a specific form of line
as the first non-empty line in the current list of lines,
and fail with an error if not found.

\begin{code}
requireKey :: (MonadFail m) => String -> Parser m Line
requireKey key [] = fail ("EOF while expecting key "++key)
requireKey key (ln@(lno,str):lns)
 | emptyLine str  = requireKey key lns
 | otherwise
    = case words str of
        [w1] | w1 == key  ->  return (ln,lns)
        _  ->  lFail lno ("Expecting '"++key++"', found:\n"++str)
\end{code}

Here, we expect something on the current line.
\begin{code}
requireKeyAndName :: (MonadFail m) => String -> Parser m (String,Line)
requireKeyAndName key [] = fail ("EOF while expecting "++key++" <name>") 
requireKeyAndName key (ln@(lno,str):lns)
  | emptyLine str  =  requireKeyAndName key lns
  | otherwise
    = case words str of
        [w1,w2] | w1 == key  ->  return ((w2,ln),lns)
        _                    ->  lFail lno ("Expecting '"++key++"' and name")
\end{code}

Here we will pass over empty lines

\begin{code}
requireKeyAndValue :: (MonadFail m) 
                   => ParseMode -> String -> Parser m (Expr,Line)
requireKeyAndValue pmode key [] 
  = fail ("EOF while expecting "++key++" <expr>")
requireKeyAndValue pmode key ((lno,str):lns)
  | emptyLine str  =  requireKeyAndValue pmode key lns
  | otherwise
    = case words str of
        (w1:wrest) | w1 == key
           ->  parseExpr pmode lns [(lno,unwords wrest)]
        _  ->  fail $ unlines
                 [ "Line "++show lno++": expecting '"++key++"' and expr"
                 , str ]

require2KeysAndValue :: (MonadFail m) 
                     => ParseMode -> String -> String -> Parser m (Expr,Line)
require2KeysAndValue pmode key1 key2 [] 
  = fail ("EOF while expecting "++key1++" "++key2++" <expr>")
require2KeysAndValue pmode key1 key2 ((lno,str):lns)
  | emptyLine str  =  require2KeysAndValue pmode key1 key2 lns
  | otherwise
    = case words str of
        (w1:w2:wrest) | w1 == key1 && w2 == key2
           ->  parseExpr pmode lns [(0,unwords wrest)]
        _  ->  fail $ unlines
                 ["Expecting '"++key1++" "++key2++"' expr"
                 ,"Found '"++str++"'" ]

\end{code}


\begin{code}
lFail lno msg = fail ("Line:"++show lno++"\n"++msg)
\end{code}


\newpage
\section{Chunk Parser}

A chunk is found by skipping over zero or more empty lines,
to find a maximal run of one or more non-empty lines.
A chunk is either followed by at least one empty line,
or the end of all of the lines.
\begin{code}
getChunk []       =  ([],[])

getChunk (ln@(_,str):lns)
 | emptyLine str  =  getChunk       lns
 | otherwise      =  getChunk' [ln] lns

getChunk' snl []  =  (reverse snl, [])

getChunk' snl (ln@(_,str):lns)
 | emptyLine str  =  (reverse snl,lns)
 | otherwise      =  getChunk' (ln:snl) lns
\end{code}


\section{Theorem Utilities}

\begin{code}
findTheorem :: (MonadFail m) => String -> [Theorem] -> m Theorem
findTheorem _ [] = fail "theorem not found"
findTheorem nm (thm:thms)
 | nm == thmName thm  =  return thm
 | otherwise          =  findTheorem nm thms
\end{code}
