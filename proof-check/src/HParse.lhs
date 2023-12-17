\chapter{Haskell Parser}
\input{Copyright}
\begin{code}
module HParse
( Line, Lines, Parser
, noline, eof, headln, headstr
, parseHModule
, parseExpr, hParseE
, parseEqual
, hs42
, ParseMode(..), ParseResult(..), SrcLoc(..), pFail
)
where
import Prelude hiding(fail)

import Data.Char
import Data.List
import qualified Data.Map as M

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax
import Control.Monad.Fail

import Utilities
import AST

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
pdbg name x = dbg (name++":\n") x
mdbg msg x = return $! dbg msg x
\end{code}

\newpage
\section{Monadic Failure}
A polymorphic, monadic parser type:
\begin{code}
type Line = (Int,String)
type Lines = [Line]
type Parser m a  = Lines -> m (a,Lines)
\end{code}

Some useful \texttt{Line} constants:
\begin{code}
noline,eof :: Line
noline = (0,"")
eof = (99999,"")
\end{code}

Sometimes we want the first line, or blank:
\begin{code}
headln :: Lines -> Line
headln [] = eof
headln (ln:_) = ln
headstr :: Lines -> String
headstr = snd . headln
\end{code}

For multi-line expressions, 
it can sometimes help to convert them to one string:
\begin{code}
lines2Line :: Lines -> Line
lines2Line [] = eof
lines2Line ((lno,s1):rest) = (lno,concat $ intersperse "\n" (s1:map snd rest))
\end{code}


From \texttt{Language.Haskell.Parser}%
\footnote{
 Has more components in \texttt{Language.Haskell.Exts.Parser},
 including fixity handling!
}%
:
\begin{verbatim}
data ParseMode = ParseMode {parseFilename :: String}
\end{verbatim}
A \texttt{SrcLoc}-based monadic failure:
\begin{code}
pFail :: (MonadFail m) 
      => ParseMode -> Int -> Int -> String -> String -> m a
pFail pmode lno colno msg text
  = fail $ unlines
      [ "@XXXX: Theory file "++pfn++" failed to parse"
      , pfn ++ ':':show lno++ ":"++show colno ++" "++msg
      , show lno++":"
      , text ]
  where pfn = parseFilename pmode
\end{code}

\section{Parser Top-Level}

\begin{code}
parseHModule :: (MonadFail m) => String -> String -> m Mdl
parseHModule fname modstr
 = case parseModuleWithMode pmode modstr of
     ParseFailed loc msg 
       -> pFail pmode (srcLine loc) (srcColumn loc) msg ""
     ParseOk hsmod -> return $ hsModule2Mdl hsmod
 where pmode = ParseMode fname
\end{code}

\newpage
\section{Parsing Expressions}

We have easy access to a complete module parser,
so parse an expression \texttt{expr} by creating a module just to hold it:
\begin{verbatim}
module NakedExpr where nakedExpr = expr
\end{verbatim}
We provide functions to wrap and unwrap that expression:
\begin{code}
mkNakedExprModule [(_,str)]
  = unlines [ "module NakedExpr where"
            , "nakedExpr = "++str ]
mkNakedExprModule chunk
  = unlines ( [ "module NakedExpr where"
              , "nakedExpr = " ]
              ++ map snd chunk )

getNakedExpr :: (MonadFail m) => HsModule -> m HsExp
getNakedExpr
 (HsModule _ _ _ _ [ HsPatBind _ _ (HsUnGuardedRhs hsexp) [] ])
    = return hsexp
getNakedExpr _ = fail "can't find the naked expression"
\end{code}

We parse an expression by using a helper 
that does the wrap, parse and unwarp part,
and then convert the result to our simpler Haskell syntax:
\begin{code}
parseExpr :: (MonadFail m) => ParseMode -> Lines -> Parser m (Expr,Line)
parseExpr pmode restlns chunk
 = do ((hsexp,ln),lns') <- hParseE pmode restlns ch1
      return ((hsExp2Expr preludeFixTab hsexp,ln),lns')
 where
   ch1 = [ lines2Line chunk ]
-- perhaps we merge these chuncks into a one-liner ????

hParseE :: (MonadFail m) => ParseMode -> Lines -> Parser m (HsExp,Line)
hParseE pmode restlns [] = pFail pmode 0 0 "no expression!"  ""
hParseE pmode restlns chunk@(ln@(lno,str):_)
  = case parseModuleWithMode pmode (mkNakedExprModule chunk) of
      ParseFailed _ msg  -> pFail pmode lno 1 msg str
      ParseOk hsmod -> do hsexp <- getNakedExpr hsmod
                          return ((hsexp,ln),restlns)
\end{code}



\section{Parsing Equivalences}

Here we parse an equivalence,
returning \texttt{42 == 42} if the parse fails.
\begin{code}
parseEqual :: (MonadFail m) => ParseMode -> Lines -> Parser m (Expr, Expr)
parseEqual pmode restlns [] = pFail pmode 0 0 "no equivalence!" ""
parseEqual pmode restlns chunk@((lno,ln):_)
  = case parseModuleWithMode pmode (mkNakedExprModule chunk) of
      ParseFailed _ msg  -> pFail pmode lno 1 msg ln
      ParseOk hsmod -> return (getNakedEqual hsmod, restlns)

getNakedEqual :: HsModule -> (Expr,Expr)
getNakedEqual
 (HsModule _ _ _ _ [ _, HsPatBind _ _ (HsUnGuardedRhs hsexp) [] ])
   = case hsexp of
       (HsInfixApp e1 (HsQVarOp (UnQual (HsSymbol "=="))) e2)
                ->  ( hsExp2Expr preludeFixTab e1
                    , hsExp2Expr preludeFixTab e2)
       _        ->  (hs42,hs42)
getNakedEqual _  =  (hs42,hs42)

hs42 = LInt 42
\end{code}
