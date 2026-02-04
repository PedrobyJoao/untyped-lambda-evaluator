{-# LANGUAGE OverloadedStrings #-}

module Interpreter (interpret) where

import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy  as TL
import           Named           (BetaReduction, ElapsedNs, EvalResult (..),
                                  Expr, Var, alphaEq, evalWithStatistics,
                                  maxEvalSteps)
import           Parser          (ParsedProgram (..), parseNoPrelude,
                                  parseWithPrelude)
import           Text.Megaparsec (errorBundlePretty)

type WithPrelude = Bool
type ResultExpr = TL.Text

-- | Parse a program in the form of:
--
-- 0 or many let bindings
-- <final expression>
--
-- Evaluates it and returns:
-- - resolved output text (may be replaced by a referenced name from env)
-- - the full EvalResult (trace + stop reason, etc.)
-- - elapsed time
interpret
  :: BetaReduction
  -> WithPrelude
  -> String
  -> IO (Either TL.Text (ResultExpr, EvalResult, ElapsedNs))
interpret strategy withPrelude exprStr = do
  let parseFn = if withPrelude then parseWithPrelude else parseNoPrelude
  case parseFn exprStr of
    Left parseErr ->
      pure $ Left (TL.pack (errorBundlePretty parseErr))

    Right parsedProgram -> do
      (evalRes, elapsedNs) <-
        evalWithStatistics maxEvalSteps strategy (parsedExpr parsedProgram)
      let result = evaluated evalRes
      let outputTxt = resolveOutputIfReferenced result (namings parsedProgram)
      pure $ Right (outputTxt, evalRes, elapsedNs)

-- ============
-- Output resolution: replace for a name if expression matches a binding
-- in the env
-- ============

resolveOutputIfReferenced :: Expr -> M.Map Var Expr -> TL.Text
resolveOutputIfReferenced result env =
  TL.pack $ maybe (show result) show (referenceName result env)

referenceName :: Expr -> M.Map Var Expr -> Maybe Var
referenceName result env =
  M.foldrWithKey
    (\v rhs acc ->
      case acc of
        Just _  -> acc
        Nothing -> if alphaEq result rhs then Just v else Nothing
    )
    Nothing
    env
