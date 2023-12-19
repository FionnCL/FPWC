\chapter{Checking}
\input{Copyright}
\begin{code}
module Check
(Report, showReport, PrfCntxt(..), checkTheorem)
where

import Utilities
import AST
import HParse
import Theory
import Matching

import Data.List (sort)

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
pdbg what x = dbg (what ++ ":\n") x
sdbg what expr = trace (what ++ ":\n"++showExpr expr) expr
mdbg msg x = return $! dbg msg x
\end{code}

\newpage
\section{Reporting}

We keep this simple --- lines of text.
\begin{code}
type Report = [String]

showReport rep = putStrLn $ unlines rep

rep :: String -> Report
rep str = lines str

rjoin :: Report -> Report -> Report
r1 `rjoin` r2 = r1 ++ r2
\end{code}

We use the following report prefixes to highlight important findings:
in reports:
\begin{code}
prfOK   = "GOOD"
prfBAD  = "***OOPS"
stepOk  = "good"
stepBAD = "***oops"

report finding (lno,_) msg
  | lno == 0   =  rep ('@':finding++": "++msg)
  | otherwise  =  rep ('@':finding++" - "++show lno++": "++msg)

repPOK = report prfOK
repSOk = report stepOk

alert finding (0,_) msg  =  rep ( '@':finding++": "++msg )
alert finding (lno,text) msg 
  = rep $ unlines' [ '@':finding++": "++msg
                   , show  lno ++ ":"
                   , text
                   ]
    
repPBAD = alert prfBAD
repSBad = alert stepBAD

fyi  msg = rep ("FYI: "++msg)
note msg = rep ("Note: "++msg)
calcrep msg = rep ('\n':msg++":")
\end{code}

\newpage
\section{Proof Context}

We need a notion of a proof-context,
that contains modules and theories in scope,
as well as keeping track of nested strategies.
\begin{code}
data PrfCntxt
  = PC { mdls :: [Mdl]
       , thrys :: [Theory]
       , supStrat :: [Strategy] -- inner-most first
       }
  deriving Show
supStrat__ f pc = pc{ supStrat = f (supStrat pc)}
supStrat_ s = supStrat__ $ const s
\end{code}

We need to add complex strategies into the proof context
when we enter them:
\begin{code}
(+++) :: PrfCntxt -> Strategy -> PrfCntxt
prfctxt +++ strat =  supStrat__ (strat:) prfctxt
\end{code}

We want to search for an enclosing strategy of a certain types,
namely induction and case-splitting.
\begin{code}
findNearestInduction :: [Strategy] -> Maybe Strategy
findNearestInduction [] = Nothing
findNearestInduction (ind@(Induction {}) : _) = Just ind
findNearestInduction (_:strats) = findNearestInduction strats

findNearestCaseSplit :: [Strategy] -> Maybe Strategy
findNearestCaseSplit [] = Nothing
findNearestCaseSplit (ind@(CaseSplit {}) : _) = Just ind
findNearestCaseSplit (_:strats) = findNearestCaseSplit strats
\end{code}

\newpage
\section{Theorem Checking}


\begin{code}
checkTheorem :: PrfCntxt -> Theorem -> Report
checkTheorem prfctxt thm
  = rep (replicate 80 '-')
     `rjoin` rep ("Checking theorem '"++thmName thm++"'")
     `rjoin` (checkStrategy prfctxt (theorem thm) $ strategy thm)
\end{code}

\subsection{Strategy Checking}

Induction and case-based strategies require a hypothesis to be passed to
the calculation checker,
while the variations of reduction don't.
In the latter case we pass in an unsatisfiable  dummy hypothesis:
\begin{code}
dummyH = LBool False
\end{code}

We are checking that the designated strategy has been followed properly.
\begin{code}
checkStrategy :: PrfCntxt -> Expr -> Strategy -> Report

checkStrategy prfctxt goal (ReduceAll calc)
  = note "Strategy: reduce all to True"
     `rjoin` checkFirst calc goal
     `rjoin` calcrep "Calculation" `rjoin` checkCalc prfctxt calc
     `rjoin` checkLast calc (LBool True)

checkStrategy prfctxt goal (ReduceLHS calc)
  = note "Strategy: reduce LHS to RHS"
     `rjoin` checkFirst calc (lhsOf goal)
     `rjoin` calcrep "Calculation" `rjoin` checkCalc prfctxt calc
     `rjoin` checkLast calc (rhsOf goal)

checkStrategy prfctxt goal (ReduceRHS calc)
  = note "Strategy: reduce RHS to LHS"
     `rjoin` checkFirst calc (rhsOf goal)
     `rjoin` calcrep "Calculation" `rjoin` checkCalc prfctxt calc
     `rjoin` checkLast calc (lhsOf goal)

checkStrategy prfctxt goal (ReduceBoth cLHS cRHS)
  = note "Strategy: reduce RHS and LHS to same"
     `rjoin` checkBothStart goal cLHS cRHS
     `rjoin` calcrep "Check LHS" `rjoin` checkCalc prfctxt cLHS
     `rjoin` calcrep "Check RHS" `rjoin` checkCalc prfctxt cRHS
     `rjoin` checkSameLast cLHS cRHS

checkStrategy prfctxt goal strat@(CaseSplit cname ccases csline)
  = note ("Strategy: Case-Split "++cname)
    `rjoin` checkCaseScheme prfctxt cname ccases
    `rjoin` calcrep "Checking case proofs..."
    `rjoin` checkCases (prfctxt+++strat) goal ccases

-- istrat must be (Induction ...)
checkStrategy prfctxt goal istrat
  = note ( "Strategy: Induction in " ++ var ++ " :: "++ typ )
     `rjoin` checkIndScheme prfctxt goal bgoal ihypo igoal var typ aln
     `rjoin` calcrep "Check Base Case..."
     `rjoin` checkStrategy prfctxt' bgoal (baseStrategy istrat)
     `rjoin` calcrep "Check Step Case..."
     `rjoin` checkStrategy prfctxt' igoal (stepStrategy istrat)
  where
    prfctxt' = prfctxt +++ istrat
    (var,typ)  =  iVar istrat
    bgoal      =  bGoal istrat
    igoal      =  iGoal istrat
    ihypo      =  assume istrat
    aln        =  assLine istrat
\end{code}


\begin{code}
checkFirst :: Calculation -> Expr -> Report
checkFirst (CALC e0 _ ln) e
  | e0 == e   =  repPOK ln  "correct first expression."
  | otherwise =  repPBAD ln "incorrect first expression."
\end{code}

\begin{code}
lastE :: Calculation -> (Expr,Line)
lastE (CALC e [] gln)     =  (e,gln)
lastE (CALC _ steps _)  
  =  (expr',ln') -- the line before  
  where (_,expr',ln') = last steps
     

checkLast :: Calculation -> Expr -> Report
checkLast calc e
  | e' == e    =  repPOK ln'  "correct last expression."
  | otherwise  =  repPBAD ln' "incorrect last expression."
  where (e',ln') = lastE calc
\end{code}

\begin{code}
checkBothStart :: Expr -> Calculation -> Calculation -> Report
checkBothStart goal (CALC gLHS _ lnLHS) (CALC gRHS _ lnRHS)
  | goal == equal gLHS gRHS  = repPOK  noline "goal lhs/rhs"
  | otherwise                = repPBAD lnRHS "(lhs = rhs) is not goal"
\end{code}

\begin{code}
checkSameLast :: Calculation -> Calculation -> Report
checkSameLast cLHS cRHS
 | lastLHS == lastRHS  =  repPOK noline "last expressions are the same."
 | otherwise           =  repPBAD lnRHS "last expressions differ."
 where
   (lastLHS,lnLHS) = lastE cLHS
   (lastRHS,lnRHS) = lastE cLHS
\end{code}

\begin{code}
equal :: Expr -> Expr -> Expr
equal e1 e2 = App (App eEq e1) e2
\end{code}

\begin{code}
lhsOf (App (App eq e1) _)
 | eq == eEq  =  e1
lhsOf e       =  e

rhsOf (App (App eq _) e2)
 | eq == eEq  =  e2
rhsOf e       =  e
\end{code}

\begin{code}
checkIndScheme :: PrfCntxt -> Expr -> Expr -> Expr -> Expr
               -> String -> String -> Line
               -> Report
checkIndScheme prfctxt goal bgoal ihypo igoal var typ aln
  = case findTheoryInds typ (thrys prfctxt) of
      Nothing -> repPBAD noline ("No Induction scheme for "++typ)
      Just indscheme
       ->  repPOK noline ("Ind Scheme '"++typ++"' valid")
           `rjoin`
           assumeRep
           `rjoin`
           note "checkIndScheme n.y.f.i."
      -- we also need to check: (see checkStrategy (Induction above))
         -- baseVal = indscheme.base
         -- bGoal = goal[baseVal/var]
         -- stepExpr isIso_to  indscheme.indStep
         -- iGoal = goal[stepExpr/var]
  where
    assumeRep = if ihypo == goal
                then repPOK aln "Induction ASSUME matches goal"
                else repPBAD aln "Induction ASSUME different from goal"
\end{code}

\begin{code}
checkCaseScheme :: PrfCntxt -> String -> [Case] -> Report
checkCaseScheme prfctxt cname ccases
  = case findTheoryCSplits cname (thrys prfctxt) of
    Nothing -> repPBAD noline ("No CaseSplit scheme for "++cname)
    Just csScheme
      ->  repPOK noline ("CaseSplit Scheme '"++cname++"' valid")
          `rjoin`
          casesRep 0 (cases csScheme) ccases

casesRep :: Int -> [Expr] -> [Case] -> Report
casesRep ncases [] [] 
  | ncases > 1  =  repPOK noline ("Found "++show ncases++" cases.")
  | otherwise   =  repPBAD noline ("Expected 2+ cases, found only "++show ncases)
casesRep count (_:_) [] 
  = repPBAD noline ("Missing cases from "++show count++" onwards")
casesRep count [] (_:_) 
  = repPBAD noline ("Extra cases from "++show count++" onwards")
casesRep count (e:es) ((n,e',_,ln):ccases)
  | n /= ncount || e /= e'
    =  (repPBAD ln $ unlines
         [ "Case number or expression differs from expected"
         , "Saw case no. "++show n
         , "Saw expression: "++showExpr e'
         ])
       `rjoin`
       casesRep ncount es ccases
  | otherwise 
    =  repPOK noline ("Case no. "++show n++" is fine")
       `rjoin`
       casesRep ncount es ccases
  where ncount = count + 1
\end{code}

\begin{code}
checkCases :: PrfCntxt -> Expr -> [Case] -> Report
checkCases prfctxt goal []  = rep "Completed case proof checking"
checkCases prfctxt goal ((cno,cexpr,strat,ln):ccases)
  = calcrep ("Checking case "++show cno++" "++showExpr cexpr)
    `rjoin`
    checkStrategy prfctxt goal strat
    `rjoin`
    checkCases prfctxt goal ccases
\end{code}


\newpage
\subsection{Calculation Checking}
We keep the best until last \dots
\begin{code}
checkCalc :: PrfCntxt -> Calculation -> Report
checkCalc prfctxt (CALC goal [] ln)  =  repPBAD ln "No steps to check"
checkCalc prfctxt (CALC goal steps ln)
  = checkSteps prfctxt goal steps

checkSteps _ _ []  = note "Check complete"
checkSteps prfctxt goal ((just,goal',ln'):steps)
  = checkStep prfctxt goal just goal' ln'
     `rjoin` checkSteps prfctxt goal' steps
\end{code}

\subsection{Justification Checking}

This is where all the heavy lifting is done:
\begin{code}
checkStep :: PrfCntxt -> Expr -> Justification -> Expr -> Line
          -> Report
\end{code}

\subsubsection{Definition Checking}

\begin{code}
checkStep prfctxt goal (BECAUSE _ (D dnm i) howused what ln) goal' gln
 = case searchMods mds dnm i of
   Nothing -> repSBad ln ("Can't find def. "++dnm++"."++show i)
   Just defn
    -> case findAndApplyDEFN (mdlsKnown mds) defn goal howused what of
       Nothing -> repSBad ln $ unlines'
                   [ "Failed to apply DEF "++dnm++"."++show i
                   -- , "  Defn    :\n"++show defn
                   , "  HowUsed : "++show howused
                   , "  At      : "++show what
                   , "  Goal    : "++showExpr goal
                   ]
       Just goal''
        -> if goal'' == goal'
           then repSOk ln ("Use of DEF "++dnm++"."++show i++" is correct.")
           else repSBad ln $ unlines'
                 [ "Use of DEF "++dnm++"."++show i++" differs."
                 -- , "  Defn     :\n"++ show defn
                 , "  HowUsed  : "++show howused
                 , "  At       : "++show what
                 , "  Got      :  "++showExpr goal'
                 , "Expected   :  "++showExpr goal''
                 ]
 where mds = mdls prfctxt
\end{code}

\newpage
\subsubsection{Law Checking}

\begin{code}
checkStep prfctxt goal (BECAUSE _ (L lnm) howused what ln) goal' gln
 = case findTheoryLaws ths lnm of
     Nothing -> repSBad ln ("Can't find LAW "++lnm)
     Just thelaw
       -> case findAndApplyLAW (mdlsKnown mds) thelaw goal howused what of
           Nothing -> repSBad ln ("Failed to apply LAW "++lnm++" "++show howused)
           Just goal''
             -> if goal'' == goal'
                 then repSOk ln ("Use of LAW "++lnm++" "++show howused++" is correct.")
                 else repSBad ln $ unlines
                       [ "Use of LAW "++lnm++" "++show howused++" differs."
                       , "What: "++ show what
                       , "Got:\n"++showExpr goal'
                       , "Expected   :  "++showExpr goal''
                       ]
 where ths = thrys prfctxt ; mds = mdls prfctxt
\end{code}

\subsubsection{If Checking}

\begin{code}
checkStep prfctxt goal (BECAUSE _ (IF i) howused what ln) goal' gln
  = case findAndApplyIF i goal howused what of
      Nothing -> repSBad ln $ unlines
                  [ "Failed to apply IF "++show i
                  -- , "  Defn    :\n"++show defn
                  , "  HowUsed : "++show howused
                  , "  At      : "++show what
                  , "  Goal    :\n"++showExpr goal
                  , ""]
      Just goal''
        -> if goal'' == goal'
            then repSOk ln ("Use of IF "++show i++" is correct.")
            else repSBad ln $ unlines
                  [ "Use of IF "++show i++" differs."
                  -- , "  Defn     :\n"++ show defn
                  , "  HowUsed  : "++show howused
                  , "  At       : "++show what
                  , "  Got      :\n"++showExpr goal'
                  , "Expected   :  "++showExpr goal''
                  , ""]
\end{code}

\newpage
\subsubsection{Guard Checking}

\begin{code}
checkStep prfctxt goal (BECAUSE _ (GRD i) howused what ln) goal' gln
  = case findAndApplyGRD i goal howused what of
      Nothing -> repSBad ln $ unlines
                  [ "Failed to apply GRDIF "++show i
                  -- , "  Defn    :\n"++show defn
                  , "  HowUsed : "++show howused
                  , "  At      : "++show what
                  , "  Goal    :\n"++showExpr goal
                  , ""]
      Just goal''
        -> if goal'' == goal'
            then repSOk ln ("Use of GRDIF "++show i++" is correct.")
            else repSBad ln $ unlines
                  [ "Use of GRDIF "++show i++" differs."
                  -- , "  Defn     :\n"++ show defn
                  , "  HowUsed  : "++show howused
                  , "  At       : "++show what
                  , "  Got      :\n"++showExpr goal'
                  , "Expected   :  "++showExpr goal''
                  , ""]
\end{code}

\subsubsection{Simplifier Checking}

\begin{code}
checkStep prfctxt goal (BECAUSE _ SMP _ _ ln) goal' gln
  = let goal'' = exprSIMP goal in
      if goal'' == goal'
      then repSOk ln  ("Use of SIMP is correct.")
      else repSBad ln $ unlines
        [ "Use of SIMP differs."
        , "  Got      :\n"++showExpr goal'
        , "Expected   :  "++showExpr goal''
        , ""]
\end{code}

\subsubsection{Normalisation Checking}

\begin{code}
checkStep prfctxt goal (BECAUSE _ (NORM op) howused what ln) goal' gln
  = let goal'' = exprNORM op goal in
      if goal'' == goal'
      then repSOk ln  ("Use of NORM is correct.")
      else repSBad ln $ unlines
        [ "Use of NORM differs."
        , "  Got      :\n"++showExpr goal'
        , "Expected   :  "++showExpr goal''
        , ""]
\end{code}


\subsubsection{Induction Hypothesis Checking}

\begin{code}

checkStep prfctxt goal (BECAUSE _ IH  howused what ln) goal' gln
  = case findNearestInduction $ supStrat prfctxt of
      Nothing -> repSBad ln "Not inside an Induction strategy"
      Just strat -> 
        case  findAndApplyLAW (mdlsKnown $ mdls prfctxt) 
                 (LAW "IH" (assume strat) ln) goal howused what of
          Nothing -> repSBad ln ("Failed to apply INDHYP " ++ howatwhat)
          Just goal'' ->
            if goal'' == goal'
            then repSOk ln ("Use of INDHYP "++howatwhat++" is correct.")
            else repSBad ln $ unlines 
                   [ "Use of INDHYP "++howatwhat++" differs."
                   , "  Got      :\n"++showExpr goal'
                   , "Expected   :  "++showExpr goal''
                   ]
 where howatwhat = show howused ++" @ "++show what
\end{code}

\subsubsection{Case Property Checking}

\begin{code}

checkStep prfctxt goal (BECAUSE _ (CA cname cno) howused what ln) goal' gln
  = case findNearestCaseSplit $ supStrat prfctxt of
      Nothing -> repSBad ln "Not inside a CaseSplit strategy"
      Just strat -> 
        case  findTheoryCSplits (csName strat) ths of
          Nothing -> repSBad ln ("No CaseSplit scheme for "++ cname)
          Just csScheme ->
            case nlookup cno $ cases csScheme of
              Nothing -> repSBad ln ("No case numbered "++show cno)
              Just caselaw ->
                case findAndApplyLAW (mdlsKnown mds) 
                              (LAW "CA" caselaw ln) goal howused what
                of
                  Nothing -> repSBad ln ("Failed to apply CASEP " ++ howatwhat)
                  Just goal'' ->
                    if (goal'' == goal')
                    then repSOk ln ("Use of CASEP "++howatwhat++" is correct.")
                    else repSBad ln ("Use of CASEP "++howatwhat++" differs.")
  where
    ths = thrys prfctxt
    mds = mdls prfctxt 
    howatwhat = show howused ++" @ "++show what
\end{code}

\newpage
\subsection{Checking Support Functions}

\subsubsection{Defined Names}


We need all names defined in imported haskell files:
\begin{code}
mdlsKnown :: [Mdl] -> [String]
mdlsKnown = concat . map mdlKnown

mdlKnown mdl = getDefined $ topdecls mdl

getDefined [] = []
getDefined (Fun (m:_)        : tdcls)  = fname m : getDefined tdcls
getDefined (Bind (Var v) _ _ : tdcls)  = v       : getDefined tdcls
getDefined (_                : tdcls)  =           getDefined tdcls
\end{code}

\begin{code}
type Definition = (Expr,Expr,[Decl])

searchMods :: [Mdl] -> String -> Int -> Maybe Definition
searchMods [] dnm i = Nothing
searchMods (mdl:mdls) dnm i
  = case searchDecls (topdecls mdl) dnm i of
      Nothing  ->  searchMods mdls dnm i
      jdefn    ->  jdefn

searchDecls :: [Decl] -> String -> Int -> Maybe Definition
searchDecls [] dnm i = Nothing
searchDecls (decl:decls) dnm i
  = case checkDecl dnm i decl of
      Nothing -> searchDecls decls dnm i
      jdefn -> jdefn
\end{code}

\begin{code}
checkDecl :: String -> Int -> Decl -> Maybe Definition

checkDecl dnm i (Bind v@(Var vnm) defn ldcls)
  | dnm == vnm && i < 2  =  Just (v,defn,ldcls)
  -- only do simple  v = e where ... binds for now

checkDecl dnm i (Fun [match])
  | dnm == fname match && i < 2
                         = Just (mkLHS dnm match,rhs match, ldecls match)
checkDecl dnm i (Fun matches)
  = do match <- nlookup i matches
       if dnm == fname match
       then Just (mkLHS dnm match,rhs match, ldecls match)
       else Nothing
checkDecl _ _ _ = Nothing

mkLHS dnm match = mkApp (Var dnm) $ lhspat match

mkApp f [] = f
mkApp f (a:as)  = mkApp (App f a) as
\end{code}

\newpage
\subsubsection{Theory Laws}

\begin{code}
findTheoryLaws :: [Theory] -> String -> Maybe Law
findTheoryLaws [] lnm = Nothing
findTheoryLaws (thry:thrys) lnm
  = case searchLaws (thLaws thry) lnm of
      Nothing  ->  findTheoryLaws thrys lnm
      jlaw     ->  jlaw

searchLaws [] lnm = Nothing
searchLaws (lw:laws) lnm
  | lawName lw == lnm  =  Just lw
  | otherwise  = searchLaws laws lnm
\end{code}

\subsubsection{Induction Schemes}

\begin{code}
findTheoryInds :: String -> [Theory] -> Maybe InductionScheme
findTheoryInds _ []  =  Nothing
findTheoryInds typ (thry:thrys)
  = case searchInds (thIndSchemes thry) typ of
      Nothing       ->  findTheoryInds typ thrys
      inds          ->  inds

searchInds [] typ        =  Nothing
searchInds (inds:indss) typ
  | indType inds == typ  =  Just inds
  | otherwise            =  searchInds indss typ
\end{code}

\begin{code}
findTheoryCSplits :: String -> [Theory] -> Maybe CaseSplitScheme
findTheoryCSplits _ []  =  Nothing
findTheoryCSplits cname (thry:thrys)
  = case searchCSplits (thCaseSchemes thry) cname of
      Nothing          ->  findTheoryCSplits cname thrys
      splits           ->  splits

searchCSplits [] cname    =  Nothing
searchCSplits (cs:css) cname
  | caseName cs == cname  =  Just cs
  | otherwise             =  searchCSplits css cname
\end{code}



\newpage
\subsubsection{Finding Designated Sub-Expressions}

These do an in-order traverse of the \texttt{goal} looking for
the sub-expression defined by \texttt{what}.
Once found, they use other arguments to rewrite that sub-expression.

\paragraph{Finding Laws}~
Once found, this uses \texttt{thelaw},
according to \texttt{howused},
to rewrite the sub-expression.
\begin{code}
findAndApplyLAW :: [String] -> Law -> Expr -> Usage -> Focus -> Maybe Expr

findAndApplyLAW knowns thelaw goal howused Top
 = applyLAW knowns howused (lawEqn thelaw) goal

-- we need to check first for guarded-ifs, which are handled differently
findAndApplyLAW knowns thelaw goal howused (At nm i)
  | nm == guardedIfName
    = case pathToIndicatedName goal nm 1 of
        Nothing -> Nothing
        Just path
         -> applyAtPathFocus 
              (applyLAW knowns howused $ lawEqn thelaw) (toGuard i path) goal
  | otherwise
    = case pathToIndicatedName goal nm i of
        Nothing -> Nothing
        Just path
         -> applyAtPathFocus 
              (applyLAW knowns howused $ lawEqn thelaw) path goal
  where
    toGuard i path = path ++ [2,1,2,i,1]
\end{code}

\begin{code}
applyLAW :: [String] -> Usage -> Expr -> Expr -> Maybe Expr

applyLAW knowns Whole thelaw expr
  = case eMatch knowns expr thelaw of
      Nothing -> Nothing
      Just _ -> Just $ LBool True

applyLAW knowns L2R (Equal lhs rhs) expr
  = case eMatch knowns expr lhs of
      Nothing -> Nothing
      Just bind -> Just $ buildReplacement bind [] rhs

applyLAW knowns R2L (Equal lhs rhs) expr
  = case eMatch knowns expr rhs of
      Nothing -> Nothing
      Just bind -> Just $ buildReplacement bind [] lhs

applyLAW _ _ _ _ = Nothing
\end{code}

\newpage
\paragraph{Finding Definitions}~
\begin{code}
findAndApplyDEFN :: [String] -> Definition -> Expr -> Usage -> Focus
                 -> Maybe Expr
findAndApplyDEFN knowns defn goal howused Top
  = applyDEFN knowns howused defn goal

-- WE'LL NEED TO HANDLE guarded-ifs HERE TOO !
findAndApplyDEFN knowns defn goal howused (At nm i)
  = case pathToIndicatedName goal nm i of
      Nothing -> Nothing
      Just path
       -> applyAtPathFocus (applyDEFN knowns howused defn) path goal

applyDEFN :: [String] -> Usage -> Definition -> Expr -> Maybe Expr

applyDEFN knowns L2R (lhs,rhs,ldcls) expr
  = case eMatch knowns expr lhs of
      Nothing -> Nothing
      Just bind -> Just $ buildReplacement bind ldcls rhs

applyDEFN knowns R2L (lhs,rhs,ldcls) expr
  = case eMatch knowns expr rhs of
      Nothing -> Nothing
      Just bind -> Just $ buildReplacement bind ldcls lhs
\end{code}

\paragraph{Finding If-Expressions}~
\begin{code}
findAndApplyIF :: Int -> Expr -> Usage -> Focus -> Maybe Expr

findAndApplyIF ifi goal howused Top
  = applyIF howused ifi goal

findAndApplyIF ifi goal howused (At nm i)
  = case pathToIndicatedName goal nm i of
      Nothing -> Nothing
      Just path
       -> applyAtPathFocus (applyIF howused ifi) path goal

applyIF howused 1 (If (LBool True) trueCase   _)  =  Just trueCase
applyIF howused 2 (If (LBool False) _ falseCase)  =  Just falseCase
applyIF _ _ e = Just (Var ("e = "++showExpr e))
\end{code}



\paragraph{Finding Guarded-Ifs}~
\begin{code}
findAndApplyGRD :: Int -> Expr -> Usage -> Focus -> Maybe Expr

findAndApplyGRD gi goal howused Top
  = applyGRD howused gi goal

findAndApplyGRD gi goal howused (At nm i)
  = case pathToIndicatedName goal nm i of
      Nothing -> Nothing
      Just path
       -> applyAtPathFocus (applyGRD howused gi) path goal

applyGRD howused i 
    (Case (Var cname) [Alt (Var pname) (GrdExpr grdrespairs) _])
  | cname == guardedIfName && pname == pWildName  
                =  findAndApplyGRDRES i grdrespairs 
applyGRD _ _ e  =  Just (Var ("e = "++showExpr e))      

findAndApplyGRDRES gi grdrespairs
  | gi < 1                   =  Nothing
  | gi > length grdrespairs  =  Nothing
  | otherwise                =  applyGRDRES (grdrespairs !! (gi-1))

applyGRDRES (LBool True,res)  =  Just res
applyGRDRES _                 =  Nothing
\end{code}



\newpage
\subsection{Focus Handling}

Consider we are looking for the $i$th occurrence of name \texttt{f}
in an expression, and it is found embedded somewhere,
and is a function name applied to several arguments:
\texttt{.... f x y z ....}.
What we want returned is a pointer to that full application,
and not just to \texttt{f}.
However, this means that the location of \texttt{f}
can be arbitrarily deep down the lefthand branch of an \texttt{App},
as the above application will parse as $@ (@ (@~f~x)~y)~z$.
If the application has path $\rho$, then the path to the
occurrence of $f$ will be $\rho \cat \seqof{1,1,1}$.
So we can delete trailing ones to get up to the correct location in this case.
However if \texttt{f} occurs in an if-expression (say),
like \texttt{if f then x else y}, then if the if-expression has path $\rho$,
then $f$ has path $\rho\cat\seqof{1}$, but this last one needs to remain.
In effect we have to tag the indices to indicate if we are branching
through an application ($@$) or some other kind of node (e.g., $if$).

We want to report paths in order as they appear when scanning the formula from left to right (inorder). This order depends on the fixity of the function.

\begin{code}
-- we only care about App vs everything else right now
data ExprBranches = AppB | OtherB deriving (Eq, Show)
type Branch = (Int,ExprBranches)
type Path = [Branch] -- identify sub-expr by sequence of branch indices
\end{code}

We now search for all occurrences of a specified name
\begin{code}
findAllNameUsage :: String  -- name being looked for
                 -> Path    -- path accumulator, initially empty
                 -> Expr    -- expression being searched
                 -> [Path]  -- all paths found to given name
-- paths returned here are reversed, with deepest index first
\end{code}
A key invariant is that the \texttt{Path} component gives
the path from the top-level to the \texttt{Expr} component.

Leaf nodes and are straightforward: 
just return the current path if it is what we are looking for.
\begin{code}
findAllNameUsage nm currPath (Var v) = if nm == v then [currPath] else []

findAllNameUsage nm currPath (Cons c) = if nm == c then [currPath] else []
\end{code}
For application nodes the case of the first expression being a variable
requires special handling, when that variable denotes an infix operator.
We need to look for the infix pattern  $@ (@ \oplus x) y$ 
that represents $x \oplus y$.
\begin{code}
findAllNameUsage nm currPath (App (App (Var v) e1) e2)
  | nm == v && isInfix nm
      = findAllNameUsage nm ((2,AppB):(1,AppB):currPath) e1
        ++
        currPath 
        : 
        findAllNameUsage nm ((2,AppB):currPath) e2

findAllNameUsage nm currPath (App (Var v) e2)
  | nm == v  = currPath : findAllNameUsage nm ((2,AppB):currPath) e2

findAllNameUsage nm currPath (App e1 e2)
  =  findAllNameUsage nm ((1,AppB):currPath) e1
  ++ findAllNameUsage nm ((2,AppB):currPath) e2
\end{code}

With the if-expression, we can be searching for the whole thing,
as well as descending into its sub-expressions.
\begin{code}
findAllNameUsage nm currPath (If e1 e2 e3)
  =  checkforIf nm
  ++ findAllNameUsage nm ((1,OtherB):currPath) e1
  ++ findAllNameUsage nm ((2,OtherB):currPath) e2
  ++ findAllNameUsage nm ((3,OtherB):currPath) e3
  where 
   checkforIf "if" = [currPath]
   checkforIf _    = []
\end{code}

Case-expressions are complex.
First, we have to deal with the guarded-if usage,
in which case we only return the (current) path to it.
\begin{code}
findAllNameUsage nm currPath (Case e alts)
  | nm == guardedIfName && e == guardedIf  =  [currPath]
\end{code}

\newpage
In other cases, we have the general case expression,
and search into both the expression and the alternatives.
\begin{code}
findAllNameUsage nm currPath (Case e alts)
  =  findAllNameUsage nm ((1,AppB):currPath) e
  ++ findAltNameUsage nm ((2,OtherB):currPath) 1 alts
\end{code}

In all other cases there are no searchable sub-expressions.
\begin{code}
findAllNameUsage nm currPath e = []
\end{code}

The case-expression's 2nd component,
alternatives,
is a complex nested structure.
\begin{eqnarray*}
   p_1  &|&  g_{11} \fun e_{11}
\\      &|&  \vdots
\\      &|&  g_{1m} \fun e_{1m}
\\ \vdots
\\ p_i  &\fun& e_{i}
\\ \vdots
\\ p_z  &|&  g_{z1} \fun e_{z1}
\\      &|&  \vdots
\\      &|&  g_{zn} \fun e_{zn}
\end{eqnarray*}
The paths from the case-expression top-level to the various parts above are:
$$
\begin{array}{|c|l|}
\hline
  p_i & \langle 2,i,1 \rangle
\\\hline
  e_i & \langle 2,i,2 \rangle
\\\hline
  g_{ij} & \langle 2,i,2,j,1 \rangle
\\\hline
  e_{ij} & \langle 2,i,2,j,2 \rangle
\\\hline
\end{array}
$$
\begin{code}
findAltNameUsage nm currPath _ [] = []
findAltNameUsage nm currPath i ((Alt e grds ldcls):alts)
  =  findAllNameUsage nm   ((1,OtherB):(i,OtherB):currPath) e
  ++ findGuardNameUsage nm ((2,OtherB):(i,OtherB):currPath) grds
  -- ++  (3,OtherB)  ldcls  not yet done
  ++ findAltNameUsage nm currPath (i+1) alts

findGuardNameUsage nm currPath (NoGrd e)
  =  findAllNameUsage nm ((1,OtherB):currPath) e
findGuardNameUsage nm currPath (GrdExpr epairs)
  =  concat $ map (findGrdExprNameUsage nm currPath) $ zip [1..] epairs

findGrdExprNameUsage nm currPath (i,(grd,res))
  =    findAllNameUsage nm ((1,OtherB):cp') grd
    ++ findAllNameUsage nm ((2,OtherB):cp') res
  where cp' = (i,OtherB):currPath
\end{code}

\begin{code}
getIth :: Int -> [a] -> Maybe a
getIth _ []      =  Nothing
getIth 1 (x:_)   =  Just x
getIth n (_:xs)  =  getIth (n-1) xs

replIth :: Int -> a -> [a] -> Maybe [a]
replIth _ _ []       =  Nothing
replIth 1 x' (x:xs)  =  Just (x':xs)
replIth n x' (x:xs)  =  do xs' <- replIth (n-1) x' xs
                           return (x:xs')
\end{code}

\newpage
Given an expression ($e$), a name ($n$), and an integer $i$,
locate the $i$th (inorder) ``effective occurence'' of $n$ in $e$.
By ``effective occurrence'' we mean that if the name is of an applied
function then we want the sub-expression that corresponds to the
application of that function to all its arguments.
For example, given $(h~f) + f~ x~ y + 1$,
the first effective occurrence of $f$ is just the $f$ that is the argument
to $h$,
while the second effective occurrence is the whole application $f~x~y$.

\begin{code}
pathToIndicatedName :: Expr -> String -> Int -> Maybe [Int]
pathToIndicatedName goal nm i
 = case findAllNameUsage nm [] goal of
      [] -> Nothing
      paths
         -> case getIth i paths of
             Nothing -> Nothing
             Just path -> Just $ reverse $ map fst $ dropWhile isApp1 path
  where
    isApp1 (1,AppB)  =  True
    isApp1 _         =  False
\end{code}

\begin{code}
applyAtPathFocus :: (Expr -> Maybe Expr) -> [Int] -> Expr -> Maybe Expr
applyAtPathFocus replace []    goal = replace goal
applyAtPathFocus replace (i:is) (App e1 e2)
  | i == 1  =  do e1' <- applyAtPathFocus replace is e1
                  return $ App e1' e2
  | i == 2  =  do e2' <- applyAtPathFocus replace is e2
                  return $ App e1 e2'
applyAtPathFocus replace (i:is) (If e1 e2 e3)
  | i == 1  =  do e1' <- applyAtPathFocus replace is e1
                  return $ If e1' e2 e3
  | i == 2  =  do e2' <- applyAtPathFocus replace is e2
                  return $ If e1 e2' e3
  | i == 3  =  do e3' <- applyAtPathFocus replace is e3
                  return $ If e1 e2 e3'
\end{code}

\begin{code}
applyAtPathFocus replace (i:is) (Case e alts)   --   2:[1,2,3,1]
  | i == 1  
     =  do e' <- applyAtPathFocus replace is e
           return $ Case e' alts
  | i == 2  
    =  do alts' <- applyAtAltsPathFocus replace is alts
          return $ Case e alts'
applyAtPathFocus replace _ _ = Nothing

applyAtAltsPathFocus :: (Expr -> Maybe Expr) -> [Int] -> [Alt] -> Maybe [Alt]
applyAtAltsPathFocus _ [] _= Nothing
applyAtAltsPathFocus replace (i:is) alts  -- [1:[2,3,1]]
  = do alti <- getIth i alts 
       alti' <- applyAtAltPathFocus replace is alti
       replIth i alti' alts

applyAtAltPathFocus :: (Expr -> Maybe Expr) -> [Int] -> Alt -> Maybe Alt
applyAtAltPathFocus _ [] _ = Nothing
applyAtAltPathFocus replace (i:is) (Alt e grds ldcls) -- 2:[3,1]
  | i == 1  =  do e' <- applyAtPathFocus replace is e
                  return $ Alt e' grds ldcls
  | i == 2  =  do grds' <- applyAtGuardPathFocus replace is grds
                  return $ Alt e grds' ldcls

applyAtGuardPathFocus :: (Expr -> Maybe Expr) -> [Int] -> Guards -> Maybe Guards
applyAtGuardPathFocus _ [] _ = Nothing  
applyAtGuardPathFocus replace (1:is) (NoGrd e)
  =  do e' <- applyAtPathFocus replace is e
        return $ NoGrd e'
applyAtGuardPathFocus replace (i:is) (GrdExpr eps)   -- 3:[1]
  =  do epi <- getIth i eps
        epi' <- applyAtGrdExprPathFocus replace is epi
        eps' <- replIth i epi' eps
        return $ GrdExpr eps'

applyAtGrdExprPathFocus _ [] _ = Nothing
applyAtGrdExprPathFocus replace (i:is) (grd,res)  -- 1:[]
  | i == 1  =  do grd' <- applyAtPathFocus replace is grd
                  return (grd',res)
  | i == 2  =  do res' <- applyAtPathFocus replace is res
                  return (grd,res')
\end{code}



Builtin-simplifier
\begin{code}
exprSIMP :: Expr -> Expr
exprSIMP (InfixApp e1 op e2)  =  applyOp (nameOf op) (exprSIMP e1) (exprSIMP e2)
exprSIMP (App e1 e2)          =  App (exprSIMP e1) (exprSIMP e2)
exprSIMP (If e1 e2 e3)        =  If (exprSIMP e1) (exprSIMP e2) (exprSIMP e3)
exprSIMP (Let dcls e)         =  Let dcls $ exprSIMP e
exprSIMP (PApp nm es)         =  PApp nm $ map exprSIMP es
exprSIMP (Case e alts)        =  Case (exprSIMP e) $ map altSimp alts
exprSIMP e                    =  e

altSimp (Alt e grds ldcls)  =  Alt (exprSIMP e) (guardSIMP grds) ldcls

guardSIMP (NoGrd e) = NoGrd $ exprSIMP e
guardSIMP (GrdExpr eps) = GrdExpr $ map exprSIMP2 eps

exprSIMP2 (e1,e2)             =  (exprSIMP e1,exprSIMP e2)
\end{code}

The fun part:
\begin{code}
applyOp "+"   (LInt x) (LInt y)  =  LInt  (x+y)
applyOp "-"   (LInt x) (LInt y)  =  LInt  (x-y)
applyOp "*"   (LInt x) (LInt y)  =  LInt  (x*y)
applyOp "div" (LInt x) (LInt y)  =  LInt (x `div` y)
applyOp "=="  (LInt x) (LInt y)  =  LBool (x==y)
applyOp "/="  (LInt x) (LInt y)  =  LBool (x/=y)
applyOp "<"   (LInt x) (LInt y)  =  LBool (x<y)
applyOp "<="  (LInt x) (LInt y)  =  LBool (x<=y)
applyOp ">"   (LInt x) (LInt y)  =  LBool (x>y)
applyOp ">="  (LInt x) (LInt y)  =  LBool (x>=y)
applyOp op e1 e2 = infixapp e1 op e2
\end{code}

Builtin-normaliser
\begin{code}
exprNORM op goal  =  fold $ sort $ flatten goal
  where
    norm = exprNORM op

    flatten (App (App (Var op') e1) e2)
      | op == op'  =  flatten (norm e1) ++ flatten (norm e2)
    flatten (App e1 e2) = [ App (norm e1) (norm e2) ]
    flatten (If e1 e2 e3) = [If (norm e1) (norm e2) (norm e3)]
    flatten (Case e alts) = [Case (norm e) alts]
    flatten (Let decls e) = [Let decls (norm e)]
    flatten e = [e]

    fold [e] = e
    fold (e1:e2:es) = fold (satop e1 e2 : es)

    satop e1 e2 = App (App (Var op) e1) e2
\end{code}