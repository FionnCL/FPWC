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

The preamble for \fn{ExN.hs}:
\begin{code}
exercisePreamble :: String -> Int -> String
exercisePreamble supplied exno
  = unlines $
      [ "module Ex"++exnostr++" where" 
      , "" ]
      ++ [supplied] ++
      [ "-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS"
      , "-- DON'T MODIFY ANYTHING ABOVE THIS LINE"
      , "" ]
  where exnostr = show exno
\end{code}

The postamble for \fn{ExN.hs}:
\begin{code}
exercisePostamble :: String
exercisePostamble = unlines
  [ ""
  , "-- add extra material below here"
  , "-- e.g.,  helper functions, test values, etc. ..."
  , "" ]
\end{code}

