\subsection{File Wrapper Generation}

\begin{code}
module
  GenWrapper(
    specPreamble
  , specBody, testGroupName
  , specPostamble
  , exercisePreamble
  , exercisePostamble
  ) 
where
\end{code}

\subsubsection{Specification Wrappers}

The preamble for \fn{Spec.hs}:
\begin{code}
specPreamble :: String -> Int -> String
specPreamble imports exno
  = unlines $
      [ "{-# LANGUAGE StandaloneDeriving #-}"
      , "module Main where"
      , "import Test.HUnit"
      , "import Test.Framework as TF (defaultMain, testGroup, Test)"
      , "import Test.Framework.Providers.HUnit (testCase)"
      , "import Ex"++exnostr ]
      ++ [imports] ++
      [ "main = defaultMain tests"
      , "tests :: [TF.Test]"
      , "tests = "
      , "  [ testGroup \"TEST Ex"++exnostr++"\" [" ]
  where exnostr = show exno
\end{code}



The body for \fn{Spec.hs}:
\begin{code}
specBody :: Int -> Int -> String
specBody exno qno
  = unlines $ map ("  "++)
      ( ("  "++testGroupName exno 1)
        :
        map ((", "++) . testGroupName exno) [2..qno] )

testGroupName exno qno = "testEx"++show exno++"Q"++show qno
\end{code}

The postamble for \fn{Spec.hs}:
\begin{code}
specPostamble :: String
specPostamble = "  ] ]\n\n"
\end{code}

\subsubsection{Test Wrappers}

\subsubsection{Exercise Wrappers}

The preamble for \fn{ExN.hs} (N in 1..4),
and \fn{Ex5Qq.thr} (q in 1..3)
\begin{code}
exercisePreamble :: String -> Int -> Int -> String
exercisePreamble supplied exno qno
  | exno `elem` [1..4]
    = unlines $
        [ "module Ex"++exnostr++" where", "" ]
        ++ [supplied] ++
        [ "-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS"
        , "-- DON'T MODIFY ANYTHING ABOVE THIS LINE"
        , "" ]
  | exno == 5
    = unlines $
        [ "THEORY Ex5Q"++show qno
        , "IMPORT-THEORY Equality"
        , "IMPORT-THEORY Boolean"
        , "IMPORT-THEORY Arithmetic"
        , "IMPORT-THEORY List"
        , "IMPORT-HASKELL List"
        , "IMPORT-HASKELL Ex5",""
        -- supplied stuff in seperate Haskell file
        , "THEOREM ex5q"++show qno ]
  | otherwise = "Preamble: unknown exercise Ex"++exnostr
  where exnostr = show exno
\end{code}

The postamble for \fn{ExN.hs}:
\begin{code}
exercisePostamble :: Int -> Int -> String
exercisePostamble exno qno
  | exno `elem` [1..4]
    = unlines
        [ ""
        , "-- add extra material below here"
        , "-- e.g.,  helper functions, test values, etc. ..."
        , "" ]
  | exno == 5
    = unlines [ "STRATEGY ReduceAll", ""
              , "  2+2 == 5", ""
              , "END ReduceAll", ""
              , "QED ex5q" ++ show qno ]
  | otherwise = "Postamble: unknown exercise Ex"++exnostr
  where exnostr = show exno
\end{code}

