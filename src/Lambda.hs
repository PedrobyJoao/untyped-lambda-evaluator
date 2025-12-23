module Lambda where

{-|

Roadmap:

1. First-order lambda representation
2. HOAS
-}

data Expr = Var Var | App Expr Expr | Lam Var Expr
  deriving (Show)

newtype Var = MkVar String
  deriving (Show, Eq)

{-|
 * Call by name *

 It reduces only the leftmost outermost expression until
 it's a lambda, and then it applies the arguments without
 reducing them.

 Expressions within the lambda term are not reduced.
-}
reduce :: Expr -> Maybe Expr
reduce (Var _)   = Nothing
reduce (Lam _ _) = Nothing
reduce (App e1 e2) = case e1 of
  Var _   -> Nothing
  App _ _ -> case reduce e1 of
    Nothing    -> Nothing
    (Just e1') -> Just (App e1' e2)
  Lam v e -> Just $ substitute v e e2

substitute :: Var -> Expr -> Expr -> Expr
substitute bounded body free = case body of
  (Var v)     -> if bounded == v then free else body
  (App e1 e2) -> App (substitute bounded e1 free) (substitute bounded e2 free)
  (Lam v e)   -> if bounded == v
                   then Lam v e
                   else Lam v (substitute bounded e free)

eval :: Expr -> Expr
eval e = case reduce e of
  Nothing        -> e
  (Just reduced) -> eval reduced
