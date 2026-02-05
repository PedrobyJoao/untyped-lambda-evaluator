module EvalWithTraceSpec (spec) where

import           Named        as N
import           Test.Hspec
import           TestFixtures

spec :: Spec
spec = do
  describe "evalWithTrace" $ do
    it "records a single step for (I a) with empty alpha-renamings (NormalOrder)" $ do
      let expr = App identityI (Var a)
          res = N.evalWithTrace N.maxEvalSteps NormalOrder expr
          finalExpr = N.evaluated res
          Trace steps = N.evalTrace res
          terminationReason = N.stopReason res

      terminationReason `shouldBe` NoMoreReductions
      shouldAlphaEq finalExpr (Var a)
      length steps `shouldBe` 1

      let [s1] = steps
      betaReduction s1 `shouldBe` NormalOrder
      shouldAlphaEq (N.before s1) expr
      shouldAlphaEq (N.after s1) (Var a)
      alphaRenamings s1 `shouldBe` []

    it "records two steps for (λx.x x)(λy.y) (Applicative)" $ do
      let expr = App (Lam x (App (Var x) (Var x))) identityI
          res = N.evalWithTrace N.maxEvalSteps Applicative expr
          finalExpr = N.evaluated res
          Trace steps = N.evalTrace res
          terminationReason = N.stopReason res

      terminationReason `shouldBe` NoMoreReductions
      shouldAlphaEq finalExpr identityI
      length steps `shouldBe` 2

      let [s1, s2] = steps

      betaReduction s1 `shouldBe` Applicative
      shouldAlphaEq (N.before s1) expr
      shouldAlphaEq (N.after s1) (App identityI identityI)
      alphaRenamings s1 `shouldBe` []

      betaReduction s2 `shouldBe` Applicative
      shouldAlphaEq (N.before s2) (App identityI identityI)
      shouldAlphaEq (N.after s2) identityI
      alphaRenamings s2 `shouldBe` []

    it "records alpha-renaming in the step when substitution would capture a free variable (NormalOrder)" $ do
      -- (λx.λy.x) y
      -- Must alpha-rename inner binder y
      let expr = App (Lam x (Lam y (Var x))) (Var y)
          res = N.evalWithTrace N.maxEvalSteps NormalOrder expr
          finalExpr = N.evaluated res
          Trace steps = N.evalTrace res
          terminationReason = N.stopReason res

      terminationReason `shouldBe` NoMoreReductions
      shouldAlphaEq finalExpr (Lam z (Var y))
      length steps `shouldBe` 1

      let [s1] = steps
      betaReduction s1 `shouldBe` NormalOrder
      shouldAlphaEq (N.before s1) expr
      shouldAlphaEq (N.after s1) (Lam z (Var y))

      -- Assert the recorded alpha-renaming payload as well
      case alphaRenamings s1 of
        [ar] -> do
          shouldAlphaEq (beforeLambda ar) (Lam y (Var x))
          shouldAlphaEq (afterLambda ar) (Lam z (Var x))
        other ->
          expectationFailure $
            "Expected exactly one AlphaRenaming, got: " ++ show other
