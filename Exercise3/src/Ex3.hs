module Ex3 where

--required for all Qs:
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

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- implement the following function (which may have runtime errors):
eval :: Dict -> CExpr -> Float
eval (Value f) = f
eval (VarNm s) = s
eval (Divide x y) = x / y
eval (MulBy x y) = x * y
eval (AddInv x) = -x
eval (Not x) = not x
eval (Dfrnt x y) = (x \= y)
eval (IsNil x)
  | x == 0 = True
  | otherwise = False

-- Q2 (8 marks)
-- implement the following function (which always returns a value):
meval :: Dict -> CExpr -> Maybe Float
meval _ _ = error "Ex3Q2: meval not yet defined"

-- Q3 (4 marks)
-- Laws of Arithmetic for this question:
--    x + 0 = x
--    0 + x = x
--    x - 0 = x
--    x - x = 0
--    x * 0 = 0
--    1 * x = x
-- The following function should implement the two laws applicable
-- for *your* CExpr datatype.
simp :: CExpr -> CExpr
simp _ = error "Ex3Q3: simp not yet defined"

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...
fst :: (a,b) -> a
fst (a,_) = a