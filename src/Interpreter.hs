{-# LANGUAGE OverloadedStrings #-}

module Interpreter
  ( interpret
  , resolveOutputIfReferenced
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy  as TL
import           Named           (BetaReduction, ElapsedNs, Expr, Trace, Var,
                                  alphaEq, evalWithStatistics)
import           Parser          (ParsedProgram (..), parseNoPrelude,
                                  parseWithPrelude)
import           Text.Megaparsec (errorBundlePretty)

type WithPrelude = Bool

-- | Parse (optionally with prelude), evaluate with the given strategy,
-- and return the rendered output (possibly replaced by a referenced name)
-- along with elapsed time.
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
