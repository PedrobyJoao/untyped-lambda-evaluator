module Named where

{-|

Roadmap:

1. First-order lambda representation
2. HOAS
-}

data Expr = Var Var | App Expr Expr | Lam Var Expr
  deriving (Show, Eq)

newtype Var = MkVar String
  deriving (Show, Eq)

eval :: Expr -> Expr
eval e = case reduce e of
  Nothing        -> e
  (Just reduced) -> eval reduced

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

-- body[bounded := free]
--
-- Note on: * Variable Capture *
--
-- Given a function application (λy.x)[x := y]:
--
-- To avoid Variable Capture we must rename the bound variable
-- so that the new replacement doesn't represent something that
-- was not intended. Afterall, if the bound variable originally
-- does not have any occurrences of bounded variables, it is not
-- valid to simply insert (by a reduction) a new bound variable
-- to the expression.
--
-- (λy.x)[x := y] -> (λy.y) would be wrong because now the second `y`
-- , which came from a function application, is artificially bounded
-- to the bound variable. That is invalid.
--
-- (λy.x)[x := y] -> (λy'.y) would be the correct format.
--
-- We should rename if:
--
-- 1. The *bound variable* appears FREE in the replacement (reminder: in [x := y],
-- `y` can be a complex expression rather than just a variable).
--
-- 2. There are occurrencens of `x` in the body at all. Otherwise, we don't need to
-- rename anything if nothing will be replaced.
substitute :: Var -> Expr -> Expr -> Expr
substitute bounded body free = case body of
  (Var v)     -> if bounded == v then free else body
  (App e1 e2) -> App (substitute bounded e1 free) (substitute bounded e2 free)
  (Lam v e)
    | bounded == v -> Lam v e -- different scope for the same var name, don't try to substitute
    | isFreeIn v free && isFreeIn bounded e -> substitute bounded (alphaRename body) free
    | otherwise -> Lam v (substitute bounded e free)

-- Given (λx.<expr>), rename all occurrences of `x`, including the bound var
alphaRename :: Expr -> Expr
alphaRename (Lam var expr) = Lam newV (go var newV expr)
  where
    newV = newVar var
    go old new e = case e of
      (Var v)     -> if v == old then Var new else e
      (App e1 e2) -> App (go old new e1) (go old new e2)
      (Lam v le)  -> if old == v
                       then Lam v le -- new scope for `v`, just ignore
                       else Lam v (go old new le)
    newVar (MkVar v) = MkVar (v ++ "'")
-- todo-minor: func should only be used with abstractions, should we use Either or Maybe?
alphaRename a         = a

isFreeIn :: Var -> Expr -> Bool
isFreeIn binder expr = case expr of
  (Var v) -> binder == v
  (App e1 e2) -> isFreeIn binder e1 || isFreeIn binder e2
  (Lam innerBinder e) -> if innerBinder == binder then False
                           else isFreeIn binder e
