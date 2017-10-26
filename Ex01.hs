{- butrfeld Andrew Butterfield -}
module Ex01 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d  =  [(k,d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

find :: Eq k => Dict k d -> k -> Maybe d
find []             _                 =  Nothing
find ( (s,v) : ds ) name | name == s  =  Just v
                         | otherwise  =  find ds name

type EDict = Dict String Double

-- Part 1 : Evaluating Expressions -- (63 marks) -------------------------------

-- Implement the following function so all 'eval' tests pass.

-- eval should return Nothing if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.

eval :: EDict -> Expr -> Maybe Double

eval d (Val x) = Just x
eval [] (Val x) = Just x
eval d (Var n) = (find d n)
eval [] (Var n) = Nothing

eval d (Mul x y)
	= case (eval d x,eval d y) of
		(Just m,Just n) -> Just (m*n)
		_ -> Nothing
eval d (Add x y)
	= case (eval d x, eval d y) of
		(Just m, Just n) -> Just (m+n)
		_ -> Nothing

eval d (Dvd x y)
	= case (eval d x, eval d y) of
		(Just m, Just n)
			-> if n == 0.0 then Nothing else Just (m/n)
		_ -> Nothing
		
eval d (Sub x y)
	= case (eval d x, eval d y) of 
		(Just m, Just n) -> Just (m-n)
		_ -> Nothing
		
eval d (Def x e1 e2)
	= case eval d e1 of
		Nothing -> Nothing
		Just v1 -> eval (define d x v1) e2
eval d e = Just 1e-99
-- Part 2 : Simplifying Expressions -- (57 marks) ------------------------------

-- Given the following code :

simp :: EDict -> Expr -> Expr
simp d (Var v)        =  simpVar d v
simp d (Add e1 e2)    =  simpAdd d   (simp d e1) (simp d e2)
simp d (Sub e1 e2)    =  simpSub d   (simp d e1) (simp d e2)
simp d (Mul e1 e2)    =  simpMul d   (simp d e1) (simp d e2)
simp d (Dvd e1 e2)    =  simpDvd d   (simp d e1) (simp d e2)
simp d (Def v e1 e2)  =  simpDef d v (simp d e1) (simp d e2)
simp _ e = e  -- simplest case, Val, needs no special treatment

-- Implement the following functions so all 'simp' tests pass.

  -- (1) see test scripts for most required properties
  -- (2) (Def v e1 e2) should simplify to e2,...
    -- (2a) .... if there is no mention of v in e2
    -- (2b) .... or any mention of v in e2 is inside another (Def v .. ..)

simpVar :: EDict -> Id -> Expr

--simpVar [] `v` -> `v`
simpVar d v = (Val 1e-99)
--simp d (Val x) = x
--simp [] (Val x) =  x
--simp d (Var n) = (find d n)
--simp [] (Var n) = Nothing

simpAdd :: EDict -> Expr -> Expr -> Expr
--simpAdd d e1 e2 
	-- = let i = simp e1
		--j -> simp e2
	--in case (i,j) of
		--(Val 0.0,e) -> e
		--(e,Val 0.0) -> e
		--_ -> Add e1’ e2’
simpAdd d e1 e2 = (Val 1e-99)

simpSub :: EDict -> Expr -> Expr -> Expr
simpSub d e1 e2 = (Val 1e-99)

simpMul :: EDict -> Expr -> Expr -> Expr
simpMul d (Val 1.0) e = e
simpMul d e (Val 1.0) = e
simpMul d e1 e2 = (Val 1e-99)

simpDvd :: EDict -> Expr -> Expr -> Expr
simpDvd d e1 e2 = (Val 1e-99)

simpDef :: EDict -> Id -> Expr -> Expr -> Expr
simpDef d v e1 e2 = (Val 1e-99)
