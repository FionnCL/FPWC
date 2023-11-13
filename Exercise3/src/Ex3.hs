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
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Data.Strict.Maybe.fromJust: Nothing"

eval :: Dict -> CExpr -> Float
eval _ (Value f) = f
eval d (VarNm s) = fromJust (find s d)
eval d (Divide x y) = (eval d x) / (eval d y)
eval d (MulBy x y) = (eval d x) * (eval d y)
eval d (AddInv x) = 0 - (eval d x) -- could do x * - 1?
eval d (Not x) = if (eval d x)==0.0 then (eval d (Value 1.0)) else (eval d x)
eval d (Dfrnt x y) = if (eval d x)/=(eval d y) then (eval d (Value 1.0)) else (eval d (Value 0.0))
eval d (IsNil x) = if eval d x==0.0 then (eval d (Value 1.0)) else (eval d (Value 0.0))

-- Q2 (8 marks)
-- implement the following function (which always returns a value):
meval :: Dict -> CExpr -> Maybe Float
meval _ (Value f) = Just f
meval d (VarNm s) = (find s d)
meval d (Divide x y)
  = case (meval d x, meval d y) of
      (Just m, Just n) -> if n==0.0 then Nothing else Just (m/n)
      _ -> Nothing
meval d (MulBy x y)
  = case (meval d x, meval d y) of
      (Just m, Just n) -> Just (m*n)
      _ -> Nothing
meval d (AddInv x)
  = case (meval d x) of
      (Just m) -> Just (0 - m)
      _ -> Nothing
meval d (Not x) 
  = case (meval d x) of
      (Just m) -> if m==0.0 then (meval d (Value 1.0)) else Just m
      _ -> Nothing
meval d (Dfrnt x y)
  = case (meval d x, meval d y) of
      (Just m, Just n) -> if m/=n then (meval d (Value 1.0)) else (meval d (Value 0.0))
      _ -> Nothing
meval d (IsNil x)
  = case (meval d x) of
      (Just m) -> if m==0.0 then (meval d (Value 1.0)) else (meval d (Value 0.0))
      _ -> Nothing

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