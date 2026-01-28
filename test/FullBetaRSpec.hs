module FullBetaRSpec (spec) where

import           Named
import           Test.Hspec
import           TestFixtures

-- Helper to run eval for both FULL beta reduction strategies
strategies :: [BetaReduction]
strategies = [Applicative, NormalOrder]

-- Run a test for both Applicative and NormalOrder
testFullBetaReductions :: String -> Expr -> Expr -> Spec
testFullBetaReductions description input expected =
  describe description $ do
    mapM_ (\s -> it (show s) $ shouldAlphaEq (eval s input) expected) strategies

spec :: Spec
spec = do
  describe "Full Beta Reduction (Confluent Strategies)" $ do

    describe "Identity combinator (I = λx.x)" $ do
      testFullBetaReductions "I a = a"
        (App identityI (Var a))
        (Var a)

      testFullBetaReductions "I I = I"
        (App identityI identityI)
        identityI

      testFullBetaReductions "I (I a) = a"
        (App identityI (App identityI (Var a)))
        (Var a)

    describe "True combinator (K = λx.λy.x)" $ do
      testFullBetaReductions "K a b = a"
        (App (App trueK (Var a)) (Var b))
        (Var a)

      testFullBetaReductions "K I a = I"
        (App (App trueK identityI) (Var a))
        identityI

    describe "False combinator (KI = λx.λy.y)" $ do
      testFullBetaReductions "KI a b = b"
        (App (App falseKI (Var a)) (Var b))
        (Var b)

      testFullBetaReductions "KI a I = I"
        (App (App falseKI (Var a)) identityI)
        identityI

    describe "S combinator" $ do
      testFullBetaReductions "S K K a = a"
        (App skkExpr (Var a))
        (Var a)

    describe "Variable capture avoidance" $ do
      -- (λx.λy.x) y should NOT capture y
      -- Expected: λy'.y (the outer y remains free, inner y is renamed)
      testFullBetaReductions "(λx.λy.x) y avoids capture"
        (App (Lam x (Lam y (Var x))) (Var y))
        (Lam (y') (Var y))

      testFullBetaReductions "(λx.λy.x y) y avoids capture"
        (App (Lam x (Lam y (App (Var x) (Var y)))) (Var y))
        (Lam (y') (App (Var y) (Var (y'))))

      -- Regression: naive alphaRename picks y' even if y' is already free in the body,
      -- which would incorrectly capture it.
      --
      -- (λx.λy.(x y')) y  ==>  λy''.(y y')   (y' must remain free)
      testFullBetaReductions "(λx.λy.x y') y avoids capture when y' is free in the body (regression)"
        (App (Lam x (Lam y (App (Var x) (Var y')))) (Var y))
        (Lam y'' (App (Var y) (Var y')))

    describe "Complex expressions" $ do
      testFullBetaReductions "(λx.x x)(λy.y) = λy.y"
        (App (Lam x (App (Var x) (Var x))) (Lam y (Var y)))
        (Lam y (Var y))

      testFullBetaReductions "((λx.λy.x y) (λz.z)) a = a"
        (App (App (Lam x (Lam y (App (Var x) (Var y)))) (Lam z (Var z))) (Var a))
        (Var a)

    describe "Already reduced expressions" $ do
      testFullBetaReductions "Variable stays as variable"
        (Var x)
        (Var x)

      testFullBetaReductions "Lambda without redex stays unchanged"
        (Lam x (Var x))
        (Lam x (Var x))

      testFullBetaReductions "Application of variables stays unchanged"
        (App (Var x) (Var y))
        (App (Var x) (Var y))

  describe "Full beta reduction: Single-step reduction order" $ do

    describe "Applicative Order (rightmost innermost first)" $ do
      -- (λx.x b) ((λy.y) a)
      -- Applicative should reduce the argument first: ((λy.y) a) -> a
      -- Result after 1 step: (λx.x b) a
      it "reduces argument before function application" $ do
        let expr = App (Lam x (App (Var x) (Var b))) (App (Lam y (Var y)) (Var a))
        shouldAlphaEqJust (applicative expr)
                           (Just (App (Lam x (App (Var x) (Var b))) (Var a)))

      -- (λx.(λy.y) x) a
      -- Applicative should reduce inside the lambda body first: (λy.y) x -> x
      -- Result after 1 step: (λx.x) a
      it "reduces inside lambda body before outer redex" $ do
        let expr = App (Lam x (App (Lam y (Var y)) (Var x))) (Var a)
        shouldAlphaEqJust (applicative expr)
                           (Just (App (Lam x (Var x)) (Var a)))

      -- ((λx.x) a) ((λy.y) b)
      -- Applicative: rightmost innermost first, so reduce ((λy.y) b) -> b
      it "reduces rightmost redex first in application" $ do
        let expr = App (App (Lam x (Var x)) (Var a)) (App (Lam y (Var y)) (Var b))
        shouldAlphaEqJust (applicative expr)
                          (Just (App (App (Lam x (Var x)) (Var a)) (Var b)))

    describe "Normal Order (leftmost outermost first)" $ do
      -- (λx.x b) ((λy.y) a)
      -- Normal order should reduce the outermost redex first
      -- Result after 1 step: ((λy.y) a) b
      it "reduces outermost redex before argument" $ do
        let chosenRedex = (App (Lam y (Var y)) (Var a))
            expr = App (Lam x (App (Var x) (Var b))) chosenRedex
        shouldAlphaEqJust (normalOrder expr)
                          (Just (App chosenRedex (Var b)))

      -- λx.(λy.y) x
      -- Normal order still reduces inside lambdas (unlike call-by-name)
      it "reduces inside lambda abstraction" $ do
        let expr = Lam x (App (Lam y (Var y)) (Var x))
        shouldAlphaEqJust (normalOrder expr)
                          (Just (Lam x (Var x)))

      -- ((λx.x) a) ((λy.y) b)
      -- Normal order: leftmost outermost first, so reduce ((λx.x) a) -> a
      it "reduces leftmost redex first in application" $ do
        let expr = App (App (Lam x (Var x)) (Var a)) (App (Lam y (Var y)) (Var b))
        shouldAlphaEqJust (normalOrder expr)
                          (Just (App (Var a) (App (Lam y (Var y)) (Var b))))
