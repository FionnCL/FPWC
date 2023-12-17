\chapter{Abstract Syntax Tree}
\input{Copyright}
\begin{code}
{-# LANGUAGE PatternSynonyms #-}
module AST
(
  Expr(..), mkName, nameOf, showExpr
, Alt(..), Guards(..)
, Match(..), Decl(..), Mdl(..), FixTab
, hsModule2Mdl, hsDecl2Decl, hsExp2Expr
, pattern InfixApp, infixapp, pattern Equal
  -- special variables:
, eNullName, eNull
, eConsName, eCons
, eEqName, eEq
, pWildName, pWild
, pAsName, pAs
, guardedIfName, guardedIf
, preludeFixTab, isInfix
)
where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

import Data.Map (Map)
import qualified Data.Map as M

import Data.Char


import Utilities

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
pdbg nm x = dbg (nm++":\n") x
mdbg msg x = return $! dbg msg x
\end{code}

\newpage
\section{Simplified Haskell AST}

We need a simplified AST for haskell.
We don't need any source-locs,
and we really don't need to distinguish identifiers from symbols,
handle qualified names,
or treat patterns differently to general expressions.
Wildcard patterns (or even irrefutable ones)
can be handled using names.


We simplify things dramatically.
First, expressions:
\begin{code}
data Expr
  = LBool Bool | LInt Int | LChar Char
  | Var String -- string starts with lowercase, or not with ':'
  | Cons String -- string starts with uppercase, or with ':'
  | App Expr Expr
  | If Expr Expr Expr
  | Case Expr [Alt]
  | Let [Decl] Expr
  | PApp String [Expr]
  deriving (Eq,Ord,Show)
\end{code}

We need to convert a name \texttt{n} to a \texttt{Var} or \texttt{Cons}
as appropriate:
\begin{code}
mkName :: String -> Expr
mKName ""       =  Cons "NONAME"
mkName nm@(c:_) 
  | isUpper c   =  Cons nm
  | c == ':'    =  Cons nm
  | nm == "[]"  =  Cons nm
  | otherwise   =  Var  nm

nameOf :: Expr -> String
nameOf (Var nm)     =  nm
nameOf (Cons nm)    =  nm
nameOf (PApp nm _)  =  nm
nameOf _            =  "NONAME"
\end{code}

Pretty-printer (attempt 1)
\begin{code}
showExpr :: Expr -> String
showExpr (LBool b) = show b 
showExpr (LInt i) = show i 
showExpr (LChar c) = show c 
showExpr (Var v) = v
showExpr (Cons c) = c 
showExpr (App (Var f) e) = f ++ " (" ++ showExpr e ++ ")"
showExpr (App (Cons c) e) = '(' : c ++ " (" ++ showExpr e ++ "))"
showExpr (App (App (Var f) e1) e2) = showBin f e1 e2
showExpr (App (App (Cons c) e1) e2) = showBin c e1 e2
showExpr (App e1 e2) = '(' : showExpr e1 ++ ") (" ++ showExpr e2 ++ ")"
showExpr (Case e alts)
  = "case "++showExpr e++" of\n" ++ (unlines' $ map showAlt alts)
showExpr e = show e

showBin op e1 e2
  | isPrefixOp op  =  op ++ " " ++ showExpr (App e1 e2)
  | otherwise  =  '(' : showExpr e1 ++ " " ++ op ++ " "++ showExpr e2 ++ ")"

isPrefixOp :: String -> Bool -- we assume string is never empty
isPrefixOp (c:_) = isLower c || c == '_'
\end{code}

Case alternatives:
\begin{code}
data Alt = Alt Expr Guards [Decl] deriving (Eq, Ord, Show)

data Guards 
  = NoGrd Expr             --      -> exp
  | GrdExpr [(Expr,Expr)]  -- | exp -> exp 
  deriving (Eq, Ord, Show)

showAlt (Alt patn grdresult decls)
  = "  " ++ showExpr patn ++ " | " ++ showGuards grdresult ++ showDecls decls

showGuards (NoGrd result) = " -> "++showExpr result
showGuards (GrdExpr epairs) = unlines' $ map showGuard epairs

showGuard (cond,result) = showExpr cond ++ " -> " ++ showExpr result
\end{code}

Next, matchings:
\begin{code}
data Match = Match { fname ::  String  -- function name
                   , lhspat :: [Expr]  -- LHS patterns
                   , rhs :: Expr    -- RHS outcome
                   , ldecls :: [Decl]  -- local declarations
                   }
           deriving (Eq, Ord, Show)
\end{code}
Then, declarations:
\begin{code}
data Decl
  = Fun [Match]
  | Bind Expr Expr [Decl]
  | Fixity String Int Assoc
  | Type String -- just noting name for now - to be addressed later
  deriving (Eq, Ord, Show)

showDecls [] = ""
showDecls (dcl:dcls) = "\n -- showDecls NYI"
\end{code}

Associativity: left, right or none:
\begin{code}
data Assoc = ANone | ALeft | ARight deriving (Eq,Ord,Show)
\end{code}

We want to be able to record fixity information:
\begin{code}
type FixTab = Map String (Int,Assoc)
\end{code}

Looking up a fixity table:
\begin{code}
readFixTab :: FixTab -> String -> (Int,Assoc)
readFixTab ftab op
  =  case M.lookup op ftab of
      Nothing   ->  (9,ALeft)  -- see H2010 Report, 4.4.2
      Just res  ->  res
\end{code}


Finally, modules (ignoring exports)
\begin{code}
data Mdl = Mdl { mname :: String
               , imps :: [Import]
               , topdecls :: [Decl]
               }
         deriving Show

data Import = Imp { imname :: String
                  , asnmame :: Maybe String
                  }
            deriving Show
\end{code}

\newpage
\section{Prelude Fixity Declarations}
We need to setup the Prelude fixities
(
\url{https://hackage.haskell.org/package/haskell-src-exts-1.23.1/docs/src/Language.Haskell.Exts.Fixity.html#preludeFixities}
)%
\footnote{
From the above link we also get base package fixities.
}%
:

\begin{code}
preludeFixTab
 = M.fromList
      [ ("!!",(9,ALeft))  -- infixl 9  !!
      , (".",(9,ARight))  -- infixr 9  .

        -- infixr 8  ^, ^^, **
      , ("^",(8,ARight)), ("^^",(8,ARight)), ("**",(8,ARight))

        -- infixl 7  *, /, `quot`, `rem`, `div`, `mod`
      , ("*",(7,ALeft)), ("/",(7,ALeft))
      , ("quot",(7,ALeft)), ("rem",(7,ALeft))
      , ("div",(7,ALeft)), ("mod",(7,ALeft))

      , ("+",(6,ALeft)), ("-",(6,ALeft))     -- infixl 6  +, -

      , (":",(5,ARight))  -- infixr 5  :
      , ("++",(5,ALeft))  -- infixl 5  ++

        -- infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
      , ("==",(4,ANone)), ("/=",(4,ANone))
      , ("<",(4,ANone)), ("<=",(4,ANone)), (">=",(4,ANone)), (">",(4,ANone))
      , ("elem",(4,ANone)), ("notElem",(4,ANone))

      , ("&&",(3,ARight))                     -- infixr 3  &&
      , ("||",(2,ARight))                     -- infixr 2  ||
      , (">>",(1,ALeft)), (">>=",(1,ALeft))   -- infixl 1  >>, >>=
      , ("=<<",(1,ARight))                    -- infixr 1  =<<

        -- infixr 0  $, $!, `seq`
      , ("$",(0,ARight)), ("$!",(0,ARight)), ("seq",(0,ARight))
      ]
isInfix :: String -> Bool
isInfix opname = opname `M.member` preludeFixTab
\end{code}

\newpage
\section{Simplifying Language.Haskell.Syntax}

\subsection{Simplifying Strings}

\begin{code}
hsName2Str :: HsName -> String
hsName2Str (HsIdent str)  = str
hsName2Str (HsSymbol str) = str

hsOpName :: HsOp -> String
hsOpName (HsVarOp hn) = hsName2Str hn
hsOpName (HsConOp hn) = hsName2Str hn

hsSpcCon2Str :: HsSpecialCon -> String
hsSpcCon2Str HsUnitCon  =  "()"
hsSpcCon2Str HsListCon  =  "[]"
hsSpcCon2Str HsFunCon   =  "->"
hsSpcCon2Str HsCons     =  ":"
hsSpcCon2Str (HsTupleCon i)  = "("++replicate (i-1) ','++")"

hsQName2Str :: HsQName -> String
hsQName2Str (Qual (Module m) nm) = m ++ '.':hsName2Str nm
hsQName2Str (UnQual nm) = hsName2Str nm
hsQName2Str (Special hsc) = hsSpcCon2Str hsc

hsQOp2Str :: HsQOp -> String
hsQOp2Str (HsQVarOp hsq)  = hsQName2Str hsq
hsQOp2Str (HsQConOp hsq)  = hsQName2Str hsq

hsExp2Str :: HsExp -> String
hsExp2Str (HsVar qnm)  = hsQName2Str qnm
hsExp2Str (HsCon qnm)  = hsQName2Str qnm
hsExp2Str hse = "hsExp2Str invalid for "++show hse

hsPat2Str :: HsPat -> String
hsPat2Str (HsPVar pnm) = hsName2Str pnm
hsPat2Str hsp = "hsPat2Str invalid for "++show hsp
\end{code}


\newpage
\subsection{Simplifying Literals}

We treat True and False as special ``literal'' constructors%
\footnote{
 Null constructors in Haskell are in fact literal values,
 semantically speaking.
}%
.
\begin{code}
hsCons2Expr :: String -> Expr
hsCons2Expr "False"  =  LBool False
hsCons2Expr "True"   =  LBool True
hsCons2Expr str      =  mkName str
\end{code}


\begin{code}
hsLit2Expr :: HsLiteral -> Expr
hsLit2Expr (HsInt i)   =  LInt $ fromInteger i
hsLit2Expr (HsChar c)  =  LChar c
hsLit2Expr (HsString s) = Var s
hsLit2Expr lit         =  LChar '?'

hsNeg2Expr i   = LInt (-(fromInteger i))
\end{code}


\subsection{Simplifying Parsed Expressions}

He we transform the rich full-featured Haskell abstract syntax 
in \texttt{HsExpr} into our simplified subset \texttt{Expr}.


\subsubsection{Simplifying \texttt{HsExpr}}

\begin{code}
hsExp2Expr :: FixTab -> HsExp -> Expr
hsExp2Expr _    (HsVar hsq)  =  mkName $ hsQName2Str hsq
hsExp2Expr _    (HsCon hsq)  =  hsCons2Expr $ hsQName2Str hsq
hsExp2Expr _    (HsLit lit)  =  hsLit2Expr lit
hsExp2Expr _    (HsNegApp (HsLit (HsInt lit))) = hsNeg2Expr lit
hsExp2Expr ftab iapp@(HsInfixApp _ _ _)  =  hsInfix2Expr ftab iapp
hsExp2Expr ftab (HsApp e1 e2)
  =  App (hsExp2Expr ftab e1) (hsExp2Expr ftab e2)
hsExp2Expr ftab (HsIf hse1 hse2 hse3)
  = If (hsExp2Expr ftab hse1) (hsExp2Expr ftab hse2) (hsExp2Expr ftab hse3)
hsExp2Expr ftab (HsParen hse)  =  hsExp2Expr ftab hse
hsExp2Expr ftab (HsList hses)  =  hsExps2Expr ftab hses
hsExp2Expr ftab (HsCase hse hsalts) = hsCase2Expr ftab hse hsalts
hsExp2Expr _ hse  =  Var ("hsExp2Expr NYfI:\n"++show hse)
\end{code}

\subsubsection{Simplifying \texttt{HsCase}}

\begin{code}
hsCase2Expr :: FixTab -> HsExp -> [HsAlt] -> Expr
hsCase2Expr ftab hse hsalts
  = Case (hsExp2Expr ftab hse) 
         (map (hsAlt2Alt ftab) hsalts)
\end{code}

\begin{code}
hsAlt2Alt :: FixTab -> HsAlt -> Alt
hsAlt2Alt ftab (HsAlt _ hsp hsgalts hsdecls)
  = Alt (hsPat2Expr hsp)
        (hsGrdAlts2Guards ftab hsgalts)
        (map (hsDecl2Decl ftab) hsdecls)

hsGrdAlts2Guards :: FixTab -> HsGuardedAlts -> Guards
hsGrdAlts2Guards ftab (HsUnGuardedAlt hse) = NoGrd $ hsExp2Expr ftab hse
hsGrdAlts2Guards ftab (HsGuardedAlts hsgalt)
  = GrdExpr $ map (hsGrdAlt2Guard ftab) hsgalt

hsGrdAlt2Guard :: FixTab -> HsGuardedAlt -> (Expr,Expr)
hsGrdAlt2Guard ftab (HsGuardedAlt _ hscond hsres)
  =  ( hsExp2Expr ftab hscond, hsExp2Expr ftab hsres )
\end{code}


Other stuff to be handled:
\begin{verbatim}
HsCase
  =  HsCase HsExp [HsAlt]

HsAlt 
  =  HsAlt SrcLoc HsPat HsGuardedAlts [HsDecl]

HsGuardedAlts 
  =  HsUnGuardedAlt HsExp	         --   -> exp
  |  HsGuardedAlts [HsGuardedAlt]  --   gdpat

HsGuardedAlt 
  =  HsGuardedAlt SrcLoc HsExp HsExp	--  | exp -> exp
\end{verbatim}

We want to match and build infix operators as simple unary applications:
\begin{code}
eEqName = "=="
eEq     = mkName eEqName
pattern InfixApp e1 op e2 = App (App op e1) e2
infixapp e1 nm e2         = InfixApp e1 (mkName nm) e2
pattern Equal e1 e2       = App (App (Var "==") e1) e2
\end{code}

\begin{code}
eNullName = "[]"
eNull     = mkName eNullName
eConsName = ":"
eCons     = mkName eConsName
hsExps2Expr :: FixTab -> [HsExp] -> Expr
hsExps2Expr _ []          =  eNull
hsExps2Expr ftab (hse:hses)
  =  infixapp (hsExp2Expr ftab hse) ":" (hsExps2Expr ftab hses)
\end{code}

\subsection{Simplifying Parsed Matches}

\begin{code}
hsMatch2Match :: FixTab -> HsMatch -> Match
hsMatch2Match ftab (HsMatch _ nm pats rhs decls)
  = Match (hsName2Str nm)
          (map hsPat2Expr pats)
          (hsRhs2Expr ftab rhs)
          (map (hsDecl2Decl ftab) decls)
\end{code}

\newpage
\subsection{Simplifying Parsed Declarations}

\begin{code}
hsDecl2Decl :: FixTab -> HsDecl -> Decl
hsDecl2Decl ftab (HsFunBind hsMatches)
  = Fun $ map (hsMatch2Match ftab) hsMatches

hsDecl2Decl ftab (HsPatBind _ hspat hsrhs hsdecls)
 = Bind (hsPat2Expr hspat)
        (hsRhs2Expr ftab hsrhs)
        (map (hsDecl2Decl ftab) hsdecls)

-- ignore type signatures and declarations for now, just note name
hsDecl2Decl ftab (HsTypeSig _ hsn _)        = Type "::"
hsDecl2Decl ftab (HsTypeDecl _ hsn _ _) = Type $ hsName2Str hsn
hsDecl2Decl ftab (HsDataDecl _ _ hsn _ _ _) = Type $ hsName2Str hsn
hsDecl2Decl ftab(HsNewTypeDecl _ _ hsn _ _ _) = Type $ hsName2Str hsn

hsDecl2Decl ftab (HsInfixDecl _ assoc p [op])
  = Fixity (hsOpName op) p (hsAssoc2Assoc assoc)
hsDecl2Decl ftab hsd = Type ("hsDecl2Decl NYIf "++show hsd)
\end{code}

\begin{code}
hsAssoc2Assoc :: HsAssoc    ->  Assoc
hsAssoc2Assoc HsAssocNone   =   ANone
hsAssoc2Assoc HsAssocLeft   =   ALeft
hsAssoc2Assoc HsAssocRight  =   ARight
\end{code}

\subsection{Simplifying Parsed Modules}

\begin{code}
hsModule2Mdl :: HsModule -> Mdl
hsModule2Mdl (HsModule _ (Module nm) _ imports decls)
  = Mdl nm
        (map hsImpDcl2Imp imports)
        (map (hsDecl2Decl ftab) decls)
  where ftab = buildFixTab preludeFixTab decls

hsImpDcl2Imp :: HsImportDecl -> Import
hsImpDcl2Imp hsID
 = Imp (hsMod2Str $ importModule hsID)
       (hsModAs2MStr $ importAs hsID)

hsMod2Str :: Module -> String
hsMod2Str (Module str) = str

hsModAs2MStr :: Maybe Module -> Maybe String
hsModAs2MStr Nothing = Nothing
hsModAs2MStr (Just m) = Just $ hsMod2Str m
\end{code}

\newpage
\subsection{Fixity Handling}

Building a fixity table on top of a pre-existing table.
\begin{code}
buildFixTab :: FixTab -> [HsDecl] -> FixTab
buildFixTab ftab []  = ftab
buildFixTab ftab (HsInfixDecl _ assoc p [op] : decls)
  =  buildFixTab (M.insert (hsOpName op) (p,hsAssoc2Assoc assoc) ftab) decls
buildFixTab ftab (_ : decls)
  =  buildFixTab ftab decls
\end{code}


\newpage
\section{Fixing Infix Parses}

The \texttt{haskell-src} package does a very lazy parsing of infix operators
that ignores operator precedence and treats every operator as left-associative.
So
\[
\texttt{e}
=
e_1 \otimes_1 e_2 \otimes_2 e_3 \otimes_3
  \dots \otimes_{n-2} e_{n-1} \otimes_{n-1} e_n
\]
where $e_1$ is not an infix application, parses as%
\footnote{
So \texttt{x:y:z:[]} parses as $((x:y):z):[]$ !
}%
\[
e_? =
( \dots ((e_1 \otimes_1 e_2) \otimes_2 e_3) \otimes_3
  \dots \otimes_{n-2} e_{n-1}) \otimes_{n-1} e_n
\]
This needs to be fixed.
It also explains why there is a \texttt{HsParen} constructor in \texttt{HsExp}!

The first consequence is that the second argument for each operator
can be independently converted,
while the longest chain formed as long as first arguments are infix operators
needs special handling.
Let the function converting \texttt{HsExp} $h$ into \texttt{Expr} $a$
be denoted by $\Sem{}$, so that $a =\Sem{h}$.
So the example above should first be transformed into two lists as follows:
\begin{eqnarray*}
  && \Sem{e_?}
\\ &=& \coz{expand $e_?$}
\\ && \Sem{( \dots ((e_1 \otimes_1 e_2) \otimes_2 e_3) \otimes_3
         \dots \otimes_{n-2} e_{n-1}) \otimes_{n-1} e_n}
\\ &=& \coz{2nd arguments convert independently}
\\ && ( \dots ((\Sem{e_1} \otimes_1 \Sem{e_2}) \otimes_2 \Sem{e_3}) \otimes_3
       \dots \otimes_{n-2} \Sem{e_{n-1}}) \otimes_{n-1} \Sem{e_n}
\\ &=& \coz{split out longest 1st-argument chain of operators}
\\ && (~\seqof{\otimes_1,\otimes_2,\otimes_3 \dots \otimes_{n-2},\otimes_{n-1}}
       ~,~
        \seqof{\Sem{e_1},\Sem{e_2},\Sem{e_3},\dots,\Sem{e_{n-1}},\Sem{e_n}}~)
\\ &=& \coz{fuse adjacent terms of operators into sub-expression,
            highest precedence first.}
\\ && \coz{top-level list will be shorter, with lowest precedence operators}
\\ && (~\seqof{\otimes_a,\otimes_b,\otimes_3 \dots \otimes_x,\otimes_y}
       ~,~
        \seqof{\Sem{e_a},\Sem{e_b},\Sem{e_c},\dots,\Sem{e_y},\Sem{e_z}}~)
\\ &=& \coz{bottom-up, for each right-associate operator, twist the tree.}
\\ && e  \quad \coz{ --- the true form of \texttt{e}}
\end{eqnarray*}
We will describe ``tree-twisting'' below.

\begin{code}
hsInfix2Expr :: FixTab -> HsExp -> Expr
-- this is usually called with iapp being a HsInfixApp
hsInfix2Expr ftab iapp
 =  e
 where
   (ops,es) = split ftab iapp
   prcf = fst . readFixTab ftab
   (ops',es') = pfusing prcf 9 (ops,es)
   assf = snd . readFixTab ftab . nameOf
   e = twist (prcf . nameOf) assf $ head $ es' -- won't be empty
\end{code}

\newpage
\subsection{Split}

We use \texttt {split} to perform the 2nd argument conversion and splitting
\begin{eqnarray*}
  && \Sem{( \dots ((e_1 \otimes_1 e_2) \otimes_2 e_3) \otimes_3
         \dots \otimes_{n-2} e_{n-1}) \otimes_{n-1} e_n}
\\ &=& \coz{2nd arguments convert independently}
\\ && ( \dots ((\Sem{e_1} \otimes_1 \Sem{e_2}) \otimes_2 \Sem{e_3}) \otimes_3
       \dots \otimes_{n-2} \Sem{e_{n-1}}) \otimes_{n-1} \Sem{e_n}
\\ &=& \coz{split out longest 1st-argument chain of operators}
\\ && (~\seqof{\otimes_1,\otimes_2,\otimes_3 \dots \otimes_{n-2},\otimes_{n-1}}
       ~,~
        \seqof{\Sem{e_1},\Sem{e_2},\Sem{e_3},\dots,\Sem{e_{n-1}},\Sem{e_n}}~)
\end{eqnarray*}
\begin{code}
split :: FixTab -> HsExp -> ( [String], [Expr] )

-- split (B e1 op e2) = ( ops ++ [op] , es ++ [e2]) where (ops,es) = split e1
-- !!!!! if hse1 is a HsParen, then we might need to leave it alone!!!
split ftab (HsInfixApp hse1 hsop hse2)
  = (ops++[op],es++[hsExp2Expr ftab hse2])
  where
    op        =  hsQOp2Str hsop
    (ops,es)  =  split ftab hse1

-- split a@(A _) = ( [], [a] )
-- !!!! if hsexp is HsParen then that is stripped off !!!
split ftab hsexp = ([],[hsExp2Expr ftab hsexp])
\end{code}

\subsection{Fuse}
We then proceed to fuse together operators of highest precedence with
their neighbouring expressions, and keep repeating until the lowest precedence
have themselves been fused.
\begin{eqnarray*}
   && (~\seqof{\otimes_1,\otimes_2,\otimes_3 \dots \otimes_{n-2},\otimes_{n-1}}
       ~,~
        \seqof{\Sem{e_1},\Sem{e_2},\Sem{e_3},\dots,\Sem{e_{n-1}},\Sem{e_n}}~)
\\ &=& \coz{fuse adjacent terms of operators into sub-expression,
            highest precedence first.}
\\ && \coz{top-level list will be shorter, with lowest precedence operators}
\\ && (~\seqof{\otimes_a,\otimes_b,\otimes_3 \dots \otimes_x,\otimes_y}
       ~,~
        \seqof{\Sem{e_a},\Sem{e_b},\Sem{e_c},\dots,\Sem{e_y},\Sem{e_z}}~)
\end{eqnarray*}
\begin{code}
pfuse :: (String -> Int) -> Int -> [String] -> [Expr] -> ([String],[Expr])
pfuse _ p [] [e] = ([],[e])
pfuse prcf p [op] [e1,e2]
  | p == prcf op  =  ([],[infixapp e1 op e2])
  | otherwise  =  ([op],[e1,e2])
pfuse prcf p (op:ops) (e1:e2:es)
  | p == prcf op  =  pfuse prcf p ops (infixapp e1 op e2 : es)
  | otherwise     =  (op:ops',e1:es')
  where (ops',es') = pfuse prcf p ops (e2:es)

pfusing :: (String -> Int) -> Int -> ([String],[Expr]) -> ([String],[Expr])
pfusing _ (-1) oes = oes
pfusing prcf p (ops,es) = pfusing prcf (p-1) $ pfuse prcf p ops es
\end{code}

\newpage
\subsection{Twist}
We now get to the point were we look for trees built with right-associative
operators, that will still be in left-associative form.
We have to ``twist'' these trees into right-associative form.
At the top-level, we process binary sub-expressions first,
and then twist the top result.
\begin{code}
-- -- we assume everything is left-infix to start.

-- InfixApp op e1 e2
twist :: (Expr -> Int) -> (Expr -> Assoc) -> Expr -> Expr
twist prcf assf (InfixApp e1 op e2)
  = twist' prcf assf (InfixApp (twist prcf assf e1) op (twist prcf assf e2))
twist prcf assf e = e

twist' prcf assf e@(InfixApp (InfixApp e1 op1 e2) op2 e3)
  | assf op1 == ARight && assf op2 == ARight && prcf op1 == prcf op2
    = InfixApp e1 op1 (insSE prcf assf op2 e2 e3 )
twist' _ _ e = e


insSE prcf assf op2 (InfixApp e4 op3 e5) e3
  | assf op3 == ARight && prcf op2 == prcf op3
    = InfixApp e4 op3 (insSE prcf assf op2 e5 e3)
insSE  _ _ op2 e2 e3 = InfixApp e2 op2 e3
\end{code}

We view righthand-sides as case-expressions of a particular form:
\begin{code}
hsRhs2Expr :: FixTab -> HsRhs -> Expr
hsRhs2Expr ftab (HsUnGuardedRhs hse)     =  hsExp2Expr ftab hse
hsRhs2Expr ftab (HsGuardedRhss grdrhss)
  =  mkGuardedIf $ map (hsGrdRHs2Expr2 ftab) grdrhss

hsGrdRHs2Expr2 :: FixTab -> HsGuardedRhs -> (Expr, Expr)
hsGrdRHs2Expr2 ftab (HsGuardedRhs _ grd rhs)
  = (hsExp2Expr ftab grd, hsExp2Expr ftab rhs)

guardedIfName = "guarded-if"
guardedIf = Var guardedIfName
mkGuardedIf epairs = Case guardedIf [Alt pWild (GrdExpr epairs) []] 
\end{code}

For now, we view patterns as expressions
\begin{code}
pWildName = "_"
pWild     = mkName pWildName
pAsName   = "@"
pAs       = mkName pAsName
hsPat2Expr :: HsPat -> Expr
hsPat2Expr (HsPVar hsn) = mkName $ hsName2Str hsn
hsPat2Expr (HsPLit lit) = hsLit2Expr lit
hsPat2Expr (HsPList hspats) = hsPats2Expr hspats
hsPat2Expr (HsPParen hspat) = hsPat2Expr hspat
hsPat2Expr (HsPInfixApp p1 op p2)
  =  infixapp (hsPat2Expr p1) (hsQName2Str op) (hsPat2Expr p2)
hsPat2Expr HsPWildCard = pWild
hsPat2Expr (HsPAsPat nm hspat)
  =  App (App pAs $ mkName $ hsName2Str nm) $ hsPat2Expr hspat
hsPat2Expr (HsPApp qnm hspats)  
  =  PApp (hsQName2Str qnm) $ map hsPat2Expr hspats

hsPat2Expr hsp = Var ("hsPat2Expr NYfI "++show hsp)

hsPats2Expr :: [HsPat] -> Expr
hsPats2Expr []  = eNull
hsPats2Expr (hspat:hspats)
  = App (App eCons $ hsPat2Expr hspat) $ hsPats2Expr hspats
\end{code}
