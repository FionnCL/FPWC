module Ex4 where

--required for Q1
data CExpr -- the expression datatype
  = Value Float -- floating-point value
  | VarNm String -- variable/identifier name
  | Divide CExpr CExpr -- divide first by second
  | MulBy CExpr CExpr -- multiplies both
  | AddInv CExpr -- numerical negation (-x)
  -- the following are boolean expressions (using numbers)
  -- the number 0.0 represents False, all others represent True.
  | Not CExpr -- logical not
  | Dfrnt CExpr CExpr -- True if both are different
  | IsNil CExpr -- True if numeric value is zero
  deriving (Eq,Ord,Show)

type Dict = [(String,Float)]
insert :: String -> Float -> Dict -> Dict
insert s f d = (s,f):d
find :: String -> Dict -> Maybe Float
find s [] = Nothing
find s ((t,f):d)
  | s==t       =  Just f
  | otherwise  =  find s d

-- required for Q2
x `incfst` _  =  x + 1
_ `incsnd` y  =  1 + y
type Thing = ([Int],[Bool])

-- required for all Qs:

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- implement the following function (which always returns a value):
mdeval :: MonadFail m => Dict -> CExpr -> m Float
mdeval _ (Value f) = return f
mdeval d (VarNm s) = return (fromJust (find s d))
-- DIVIDE PROBABLY NEEDS ERROR HANDLING
mdeval d (Divide x y)
  = do  a <- mdeval d x
        b <- mdeval d y
        if b==0.0
          then fail ("Cannot divide by zero!")
          else return (a/b)
mdeval d (MulBy x y)
  = do  a <- mdeval d x
        b <- mdeval d y
        return (a*b)
mdeval d (AddInv x)
  = do  a <- mdeval d x
        return (0-a)
mdeval d (Not x) 
  = do  isNot <- mdeval d x
        if isNot==0.0
          then (mdeval d (Value 1.0)) 
          else (mdeval d (Value 0.0)) 
mdeval d (Dfrnt x y)
  = do  expr1 <- mdeval d x
        expr2 <- mdeval d y
        if expr1/=expr2
          then (mdeval d (Value 1.0)) 
          else (mdeval d (Value 0.0)) 
mdeval d (IsNil x)
  = do  isNot <- mdeval d x
        if isNot==0.0
          then (mdeval d (Value 1.0)) 
          else (mdeval d (Value 0.0)) 

-- Q2 (8 marks)
-- Consider the following four recursive pattern definitions:
len :: Int -> [Int] -> Int
len z []     = z
len z (x:xs) = x `incsnd` (len z xs)
sumup :: Int -> [Int] -> Int
sumup sbase []     = sbase
sumup sbase (n:ns) = n + (sumup sbase ns)
prod :: Int -> [Int] -> Int
prod mbase []     = mbase
prod mbase (n:ns) = n * (prod mbase ns)
cat :: [Thing] -> [[Thing]] -> [Thing]
cat pfx []     = pfx
cat pfx (xs:xss) = xs ++ (cat pfx xss)

-- They all have the same abstract pattern,
-- as captured by the following Higher Order Function (HOF):
foldR z _ [] = z
foldR z op (x:xs) = x `op` foldR z op xs

-- We can gather the `z` and `opr` arguments into a tuple: (op,z)
-- which allows us to construct a call to foldR as:
dofold (op,z) = foldR z op

-- Your task is to complete the tuples below,
-- so that `dofold` can be used to implement the fns. above.

-- dofold lenTuple = len
lenTuple :: (Int -> Int -> Int,Int)
lenTuple = ((+), 0)

-- dofold sumupTuple = sumup
sumupTuple :: (Int -> Int -> Int,Int)
sumupTuple = ((+), 0)

-- dofold prodTuple = prod
prodTuple :: (Int -> Int -> Int,Int)
prodTuple = ((*), 1)

-- dofold catTuple = cat
catTuple :: ([Thing] -> [Thing] -> [Thing],[Thing])
catTuple = ((++),[])

-- Q3 (11 marks)
sub = subtract -- shorter!
ops = [(25-),(26-),(24-),(*22),(31-),(+29),(*20),(22-),(sub 22),(sub 23)]

-- (!) This question requires modifying Main.hs
-- See, and/or compile and run Main.hs for further details

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Data.Strict.Maybe.fromJust: Nothing"
