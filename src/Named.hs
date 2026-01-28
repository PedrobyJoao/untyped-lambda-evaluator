module Named where

import           Data.Char (isDigit)
import           Data.List (elemIndex)

data Expr = Var Var | App Expr Expr | Lam Var Expr
  deriving (Eq)

newtype Var = MkVar String
  deriving (Eq)

data BetaReduction = Applicative | NormalOrder | CallByName
  deriving (Show, Eq)

instance Show Expr where
  show (Var v)     = show v
  show (Lam v e) = "\\" ++ show v ++ "." ++ show e
  show (App e1 e2)
    -- Var Var = a b
    -- Var Lam = a (\x.x)
    -- Var App = a (b c)
    | (Var _) <- e1, (Var _) <- e2 = show e1 ++ " " ++ show e2
    | (Var _) <- e1 = show e1 ++ parens(show e2)
    -- Lam Var = (λx.x) b
    -- Lam Lam = (\x.x) (\y.y)
    -- Lam App = (\x.x) (b c)
    -- App Var = (a b) c
    -- App Lam = (a b) (\x.x)
    -- App App = (a b) (c d)
    | (Var _) <- e2 = parens(show e1) ++ show e2
    | otherwise = parens(show e1) ++ parens(show e2)
      where parens e = "(" ++ e ++ ")"

instance Show Var where
  show (MkVar v) = v


eval :: BetaReduction -> Expr -> Expr
eval br expr = go expr
  where go e = maybe e go (reduce e)
        reduce = reduceFn br

reduceFn :: BetaReduction -> (Expr -> Maybe Expr)
reduceFn CallByName  = callByName
reduceFn Applicative = applicative
reduceFn NormalOrder = normalOrder

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
applicative :: Expr -> Maybe Expr
applicative (Var _)   = Nothing
applicative (Lam v e) = Lam v  <$> applicative e
applicative (App e1 e2)
  | Just e2' <- applicative e2 = Just $ App e1 e2'
  | Just e1' <- applicative e1 = Just $ App e1' e2
  | Lam v body <- e1 = Just $ substitute v body e2
  | otherwise = Nothing

-- * Normal Order *
--
-- Reduces first the leftmost outermost redex.
-- So arguments are replaced into functions before
-- being applied.
--
-- `(\x.x b) ((\y.y) a)`
-- 1 step: `((\y.y) a) b` ...
normalOrder :: Expr -> Maybe Expr
normalOrder (Var _)   = Nothing
normalOrder (Lam v b) = Lam v <$> normalOrder b
normalOrder (App e1 e2)
  | Lam v b <- e1 = Just $ substitute v b e2
  | Just e1' <- normalOrder e1 = Just $ App e1' e2
  | Just e2' <- normalOrder e2 = Just $ App e1 e2'
  | otherwise = Nothing

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

-- =========================
-- Alpha renaming
-- =========================

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
    | isFreeIn v arg && isFreeIn binder e ->
        let newV = fetchFreshVar v e arg
        in substitute binder (alphaRename newV body) arg
    | otherwise -> Lam v (substitute binder e arg)

-- Choose a fresh variable name for alpha-renaming a binder `oldBinder` in `body`,
-- with respect to a substitution argument `arg`.
--
-- Requirements:
-- 1) The new binder must not occur anywhere in the body (free OR as a binder),
--    otherwise alpha-renaming can introduce capture (e.g. \x.(\x_1.x)).
-- 2) The new binder must not be free in `arg`, otherwise the renamed binder would
--    capture free occurrences coming from the replacement argument.
fetchFreshVar :: Var -> Expr -> Expr -> Var
fetchFreshVar oldBinder body arg = go (nextVar oldBinder)
  where
    go candidate
      | occursAnywhere candidate body = go (nextVar candidate)
      | isFreeIn candidate arg        = go (nextVar candidate)
      | otherwise                    = candidate

-- Given (λx.<expr>), rename all occurrences of `x`, including the bound var,
-- using the *provided* new binder variable.
--
-- Important: this function does NOT choose the new name; callers must supply
-- a fresh name (see `fetchFreshVar`).
alphaRename :: Var -> Expr -> Expr
alphaRename newBinder (Lam oldBinder expr) = Lam newBinder (go oldBinder newBinder expr)
  where
    go old new e = case e of
      (Var v)     -> if v == old then Var new else e
      (App e1 e2) -> App (go old new e1) (go old new e2)
      (Lam v le)  -> if old == v
                       then Lam v le -- new scope for `v`, just ignore
                       else Lam v (go old new le)
-- todo-minor: func should only be used with abstractions, should we use Either or Maybe?
alphaRename _ a = a

isFreeIn :: Var -> Expr -> Bool
isFreeIn binder expr = case expr of
  (Var v) -> binder == v
  (App e1 e2) -> isFreeIn binder e1 || isFreeIn binder e2
  (Lam innerBinder e) -> if innerBinder == binder then False
                           else isFreeIn binder e

-- true if the variable name appears anywhere (free OR as a binder)
occursAnywhere :: Var -> Expr -> Bool
occursAnywhere x expr = case expr of
  Var v      -> v == x
  App e1 e2  -> occursAnywhere x e1 || occursAnywhere x e2
  Lam v body -> v == x || occursAnywhere x body

-- Increment a variable name using the scheme:
-- x -> x_1
-- x_1 -> x_2
nextVar :: Var -> Var
nextVar (MkVar s) =
  case parseNumericSuffix s of
    Just (base, n) -> MkVar (base ++ "_" ++ show (n + 1))
    Nothing        -> MkVar (s ++ "_1")
  where
    -- If the string ends with _<digits>, return (base, number).
    -- Example: "x_9" -> Just ("x", 9)
    --          "x"   -> Nothing
    parseNumericSuffix :: String -> Maybe (String, Int)
    parseNumericSuffix str =
      let (revDigits, rest) = span isDigit (reverse str)
      in case (revDigits, rest) of
           ([], _) -> Nothing
           (_, '_':revBase) ->
             let base = reverse revBase
                 nStr = reverse revDigits
             in Just (base, read nStr)
           _ -> Nothing

-- =========================
-- Alpha equivalence via De Bruijn indices
-- =========================

data DB
  = DBFree Var
  | DBBound Int
  | DBApp DB DB
  | DBLam DB
  deriving (Eq, Show)

-- Convert a named expression to De Bruijn indices.
-- Bound variables become DBBound n, where n=0 refers to the nearest lambda binder.
-- Free variables remain named (DBFree).
toDB :: Expr -> DB
toDB = go []
  where
    -- env: nearest binder first
    go :: [Var] -> Expr -> DB
    go env expr
      | Var v <- expr = case elemIndex v env of
          Just i  -> DBBound i
          Nothing -> DBFree v
      | App e1 e2 <- expr = DBApp (go env e1) (go env e2)
      | Lam v body <- expr = DBLam (go (v : env) body)

-- alpha equivalence: compare two expressions modulo renaming of bound variables.
alphaEq :: Expr -> Expr -> Bool
alphaEq e1 e2 = toDB e1 == toDB e2
