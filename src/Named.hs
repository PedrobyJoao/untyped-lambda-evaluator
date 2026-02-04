{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Named where

import           Control.DeepSeq             (NFData, force)
import           Control.Exception           (evaluate)
import           Control.Monad.Writer.Strict
import           Data.Char                   (isDigit)
import qualified Data.DList                  as DL
import           Data.List                   (elemIndex, intercalate)
import qualified Data.Set                    as S
import           Data.Word                   (Word64)
import           GHC.Clock                   (getMonotonicTimeNSec)
import           GHC.Generics                (Generic)

data Expr = Var Var | App Expr Expr | Lam Var Expr
  deriving (Eq, Generic, NFData)

newtype Var = MkVar String
  deriving (Eq, Ord, Generic, NFData)

data BetaReduction = Applicative | NormalOrder | CallByName
  deriving (Show, Eq, Generic, NFData)

data Trace = Trace [Step]
  deriving (Eq, Generic, NFData)

data Step = Step
  { before         :: !Expr
  , after          :: !Expr
  , betaReduction  :: !BetaReduction
  , alphaRenamings :: ![AlphaRenaming]
  }
  deriving (Eq, Show, Generic, NFData)

data AlphaRenaming = AlphaRenaming
  { prevBinder   :: !Var
  , newBinderVar :: !Var
  , beforeLambda :: !Expr
  , afterLambda  :: !Expr
  }
  deriving (Eq, Show, Generic, NFData)

data EvalStopReason
  = NoMoreReductions
  | StepLimitReached StepsLimit
  deriving (Eq, Show, Generic, NFData)

data EvalResult = EvalResult
  { evaluated  :: !Expr
  , evalTrace  :: !Trace
  , stopReason :: !EvalStopReason
  }
  deriving (Eq, Show, Generic, NFData)

-- maximum number of evaluation steps
type StepsLimit = Int
type ElapsedNs = Word64

-- Hard limit to avoid non-terminating evaluations on the server
-- todo: test a big expr
maxEvalSteps :: StepsLimit
maxEvalSteps = (2 :: Int) ^ (12 :: Int)

eval :: BetaReduction -> Expr -> Expr
eval br expr = evaluated $ evalWithTrace maxEvalSteps br expr

evalWithTrace :: StepsLimit -> BetaReduction -> Expr -> EvalResult
evalWithTrace stepsLimit br expr = go 0 expr []
  where
    reduce = reduceFn br

    go :: Int -> Expr -> [Step] -> EvalResult
    go n e acc -- n == number of steps done
      | n >= stepsLimit =
          EvalResult
            { evaluated = e
            , evalTrace = Trace (reverse acc)
            , stopReason = StepLimitReached stepsLimit
            }
      | otherwise =
          case reduce e of
            Nothing ->
              EvalResult
                { evaluated = e
                , evalTrace = Trace (reverse acc)
                , stopReason = NoMoreReductions
                }
            Just (e', ars) ->
              let step =
                    Step
                      { before = e
                      , after = e'
                      , betaReduction = br
                      , alphaRenamings = ars
                      }
              in go (n + 1) e' (step : acc)

-- Runs evalWithTrace, forces the result, and measures elapsed time using a monotonic clock.
evalWithStatistics :: StepsLimit -> BetaReduction -> Expr -> IO (EvalResult, ElapsedNs)
evalWithStatistics stepsLimit br expr = do
  t0 <- getMonotonicTimeNSec
  let res = evalWithTrace stepsLimit br expr
  _ <- evaluate (force res)
  t1 <- getMonotonicTimeNSec
  pure (res, t1 - t0)

reduceFn :: BetaReduction -> (Expr -> Maybe (Expr, [AlphaRenaming]))
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
applicative :: Expr -> Maybe (Expr, [AlphaRenaming])
applicative (Var _) = Nothing
applicative (Lam v e) = do
  (e', ars) <- applicative e
  Just (Lam v e', ars)
applicative (App e1 e2)
  | Just (e2', ars) <- applicative e2 = Just (App e1 e2', ars)
  | Just (e1', ars) <- applicative e1 = Just (App e1' e2, ars)
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
normalOrder :: Expr -> Maybe (Expr, [AlphaRenaming])
normalOrder (Var _) = Nothing
normalOrder (Lam v b) = do
  (b', ars) <- normalOrder b
  Just (Lam v b', ars)
normalOrder (App e1 e2)
  | Lam v b <- e1 = Just $ substitute v b e2
  | Just (e1', ars) <- normalOrder e1 = Just (App e1' e2, ars)
  | Just (e2', ars) <- normalOrder e2 = Just (App e1 e2', ars)
  | otherwise = Nothing

-- * Call by name *
--
-- It reduces only the leftmost outermost expression until
-- it's a lambda, and then it applies the arguments without
-- reducing them.
--
-- Expressions within the lambda term are not reduced.
callByName :: Expr -> Maybe (Expr, [AlphaRenaming])
callByName (Var _) = Nothing
callByName (Lam _ _) = Nothing
callByName (App e1 e2) = case e1 of
  Var _   -> Nothing
  App _ _ -> (\(e1', ars) -> (App e1' e2, ars)) <$> callByName e1
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
substitute :: Var -> Expr -> Expr -> (Expr, [AlphaRenaming])
substitute binder body arg =
  let (e, logDL) = runWriter (go body)
  in (e, DL.toList logDL)
  where
    go :: Expr -> Writer (DL.DList AlphaRenaming) Expr
    go b = case b of
      Var v -> pure $ if binder == v then arg else b
      App e1 e2 -> App <$> go e1 <*> go e2
      lam@(Lam v lamBody)
        | binder == v -> pure lam -- new binder context with same name, skip
        | isFreeIn v arg && isFreeIn binder lamBody -> do
            let newV = fetchFreshVar v lamBody arg
                renamedLam = alphaRename newV lam
            tell $ DL.singleton AlphaRenaming
              { prevBinder = v
              , newBinderVar = newV
              , beforeLambda = lam
              , afterLambda = renamedLam
              }
            go renamedLam
        | otherwise -> Lam v <$> go lamBody

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
      | otherwise                     = candidate

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
  (Lam innerBinder e) ->
    if innerBinder == binder then False
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
  deriving (Eq, Show, Generic, NFData)

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

-- =========================
-- Printing
-- =========================

instance Show Trace where
  show (Trace steps) = intercalate "\n\n" (map showStep steps)
    where
      showStep :: Step -> String
      showStep Step{after, alphaRenamings} =
        intercalate "\n" $
          ("β→ " ++ show after) : concatMap showAlpha alphaRenamings

      showAlpha :: AlphaRenaming -> [String]
      showAlpha AlphaRenaming{beforeLambda, afterLambda} =
        let prefix = "α→ "
            indent = replicate 2 ' '
        in [ indent ++ prefix ++ show beforeLambda
           , indent ++ "==> " ++ show afterLambda
           ]

instance Show Expr where
  show (Var v)     = show v
  show (Lam v e) = "\\" ++ show v ++ ". " ++ show e
  show (App e1 e2)
    -- Var Var = a b
    -- Var Lam = a (\x.x)
    -- Var App = a (b c)
    | (Var _) <- e1, (Var _) <- e2 = show e1 ++ " " ++ show e2
    | (Var _) <- e1 = show e1 ++ parens (show e2)
    -- Lam Var = (λx.x) b
    -- Lam Lam = (\x.x) (\y.y)
    -- Lam App = (\x.x) (b c)
    -- App Var = (a b) c
    -- App Lam = (a b) (\x.x)
    -- App App = (a b) (c d)
    | (Var _) <- e2 = parens (show e1) ++ show e2
    | otherwise = parens (show e1) ++ parens (show e2)
      where parens e = "(" ++ e ++ ")"

instance Show Var where
  show (MkVar v) = v


-- =========================
-- Helpers
-- =========================

-- todo: tests
freeVars :: Expr -> S.Set Var
freeVars = go S.empty
  where
    go :: S.Set Var -> Expr -> S.Set Var
    go bound e = case e of
      Var v      -> if v `S.member` bound then S.empty else S.singleton v
      App e1 e2  -> go bound e1 `S.union` go bound e2
      Lam v body -> go (S.insert v bound) body
