module Named where


data Expr = Var Var | App Expr Expr | Lam Var Expr
  deriving (Show, Eq)

newtype Var = MkVar String
  deriving (Show, Eq)

data BetaReduction = Applicative | NormalOrder
  | CallByName | CallByValue
  deriving (Show, Eq)

eval :: BetaReduction -> Expr -> Expr
eval br expr = go expr
  where go e = maybe e go (reduce e)
        reduce = reduceFn br

reduceFn :: BetaReduction -> (Expr -> Maybe Expr)
reduceFn CallByName  = callByName
reduceFn Applicative = applicative
reduceFn _           = nullReduction -- TODO: return error instead

nullReduction :: Expr -> Maybe Expr
nullReduction _ = Nothing

-- * Applicative *
--
-- Reduces first the rightmost innermost redex.
-- Thus arguments are reduced before being applied
-- to their related function.
--
-- Important: Applicative reductions might not terminate
-- while Normal Order reductions is guaranteed to.
--
-- `(\x.x b) ((\y.y) a)`
-- 1 step: `(\x.x b) a`
--
-- (λx.(λy.y) x) a
-- case applicative a of
-- Nothing -> case e1 of
-- Lam v e -> substitute v innerReduce a
-- innerReduce = applicative (λy.y) x
applicative :: Expr -> Maybe Expr
applicative (Var _)   = Nothing
applicative (Lam v e) = Lam v  <$> applicative e
applicative (App e1 e2)
  | Just e2' <- applicative e2 = Just $ App e1 e2'
  | Just e1' <- applicative e1 = Just $ App e1' e2
  | Lam v body <- e1 = Just $ substitute v body e2
  | otherwise = Nothing
-- my first ugly version:
-- applicative (App e1 e2) = case applicative e2 of
--   Nothing        -> case (applicative e1, e1) of
--     (Nothing, Lam v e) -> Just $ substitute v e e2
--     (Nothing, _)       -> Nothing
--     (Just e, _)        -> Just $ App e e2
--   Just e2Reduced -> Just $ App e1 e2Reduced

-- * Normal Order *
--
-- Reduces first the leftmost outermost redex.
-- So arguments are replaced into functions before
-- being applied.
--
-- `(\x.x b) ((\y.y) a)`
-- 1 step: `((\y.y) a) b` ...
normalOrder :: Expr -> Maybe Expr
normalOrder _ = Nothing

-- * Call by name *
--
-- It reduces only the leftmost outermost expression until
-- it's a lambda, and then it applies the arguments without
-- reducing them.
--
-- Expressions within the lambda term are not reduced.
callByName :: Expr -> Maybe Expr
callByName (Var _)   = Nothing
callByName (Lam _ _) = Nothing
callByName (App e1 e2) = case e1 of
  Var _   -> Nothing
  App _ _ -> (\e1' -> App e1' e2) <$> callByName e1
  Lam v e -> Just $ substitute v e e2

-- body[binder := arg]
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
substitute binder body arg = case body of
  (Var v)     -> if binder == v then arg else body
  (App e1 e2) -> App (substitute binder e1 arg) (substitute binder e2 arg)
  (Lam v e)
    | binder == v -> Lam v e -- different scope for the same var name, don't try to substitute
    | isFreeIn v arg && isFreeIn binder e -> substitute binder (alphaRename body) arg
    | otherwise -> Lam v (substitute binder e arg)

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
