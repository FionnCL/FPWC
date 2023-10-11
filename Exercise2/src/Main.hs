import Ex2

main
  = putStrLn $ unlines
      [ "Running Exercise2"
      , "f1 [1..1000] = " ++ show (f1 [1..1000])
      , "f2 [1..1000] = " ++ show (f2 [1..1000])
      , "f3 [1..1000] = " ++ show (f3 [1..1000])
      , "f4 [Just 42] = " ++ show (f4 [Just 42])
      , "f5 [Just 42] = " ++ show (f5 [Just 42])
      ]
