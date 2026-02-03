{-# LANGUAGE OverloadedStrings #-}

module Interpreter (interpret) where

import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy  as TL
import           Named           (BetaReduction, ElapsedNs, Expr, Trace, Var,
                                  alphaEq, evalWithStatistics)
import           Parser          (ParsedProgram (..), parseNoPrelude,
                                  parseWithPrelude)
import           Text.Megaparsec (errorBundlePretty)

type WithPrelude = Bool

-- | Parse a program in the form of:
--
-- 0 or many let bindings
-- <final expression>
--
-- Evaluates it and returns the resultant expression, trace and elapsed time
--
-- TODO: our e2e tests
interpret
  :: BetaReduction
  -> WithPrelude
  -> String
  -> IO (Either TL.Text (TL.Text, Trace, ElapsedNs))
interpret strategy withPrelude exprStr = do
  let parseFn = if withPrelude then parseWithPrelude else parseNoPrelude
  case parseFn exprStr of
    Left parseErr ->
      pure $ Left (TL.pack (errorBundlePretty parseErr))

    Right parsedProgram -> do
      (result, _trace, elapsedNs) <-
        evalWithStatistics strategy (parsedExpr parsedProgram)
      let outputTxt = resolveOutputIfReferenced result (namings parsedProgram)
      pure $ Right (outputTxt, _trace, elapsedNs)

-- ============
-- Output resolution: replace for a name if expression matches a binding
-- in the env
-- ============

-- todo: tests?
resolveOutputIfReferenced :: Expr -> M.Map Var Expr -> TL.Text
resolveOutputIfReferenced result env =
  TL.pack $ maybe (show result) show (referenceName result env)

-- todo: tests?
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
