module NamedCallByNameSpec where

import           Named
import           Test.Hspec
import           TestFixtures

spec :: Spec
spec = do
  describe "eval (call by name)" $ do
    it "returns a variable as-is" $ do
      shouldAlphaEq (eval CallByName (Var x))
                               (Var x)

    it "returns a lambda as-is" $ do
      shouldAlphaEq (eval CallByName identityI)
                               (identityI)

    it "applies identityI to a variable" $ do
      -- (λx.x) y → y
      shouldAlphaEq (eval CallByName (App identityI (Var y)))
                               (Var y)

    it "applies identityI to identityI" $ do
      -- (λx.x) (λx.x) → λx.x
      shouldAlphaEq (eval CallByName (App identityI identityI))
                               (identityI)

    it "applies const to two arguments" $ do
      -- (λx.λy.x) a b → a
      let la = Var (MkVar "a")
          lb = Var (MkVar "b")
      shouldAlphaEq (eval CallByName (App (App trueK la) lb))
                               (la)

    it "does not reduce under lambdas" $ do
      -- λx.(λy.y) x should NOT reduce to λx.x
      -- call by name stops at the outer lambda
      let inner = App (Lam y (Var y)) (Var x)
          expr = Lam x inner
      shouldAlphaEq (eval CallByName expr)
                               (expr)

    it "does not evaluate arguments when applying to a func" $ do
      -- (λx.λy. x y) ((λz.z) a)
      -- The argument ((λz.z) a) is substituted for x, giving:
      -- λy. ((λz.z) a) y
      -- Call by name does not reduce under lambda nor args under application,
      -- thus:
      let arg = App (Lam z (Var z)) (Var a)  -- (λz.z) a — a redex
          func = Lam x (Lam y (App (Var x) (Var y)))  -- λx.λy. x y
          expr = App func arg
          expected = Lam y (App arg (Var y))  -- λy. ((λz.z) a) y — arg still unevaluated
      shouldAlphaEq (eval CallByName expr) (expected)

    it "handles nested applications" $ do
      -- ((λx.x) (λy.y)) z → z
      shouldAlphaEq (eval CallByName (App (App identityI (Lam y (Var y))) (Var z)))
                               (Var z)

    it "handles self-application on simple terms" $ do
      -- (λx.x x) (λy.y) → (λy.y) (λy.y) → λy.y
      let selfApp = Lam x (App (Var x) (Var x))
      shouldAlphaEq (eval CallByName (App selfApp identityI))
                               (identityI)

    it "avoids variable capture during evaluation" $ do
      -- (λx.λy.x) y → λy'.y
      -- The inner binder must be renamed to avoid capturing the free y
      let expr = App (Lam x (Lam y (Var x))) (Var y)
          result = eval CallByName expr
          Lam boundVar body = result
      shouldAlphaEq body $ Var y
      boundVar `shouldNotBe` y

  describe "callByName (single step reduction)" $ do
    it "reduces leftmost redex first" $ do
      -- (λx.x) y ((λz.z) w)
      -- Two redexes: (λx.x) y and (λz.z) w
      -- Should reduce left one first: y ((λz.z) w)
      let leftRedex = App (Lam x (Var x)) (Var y)
          rightRedex = App (Lam z (Var z)) (Var w)
          expr = App leftRedex rightRedex
      shouldAlphaEqJust (callByName expr)
                        (Just (App (Var y) rightRedex))

    it "reduces outermost redex before inner redex" $ do
      -- (λx.x) ((λy.y) z)
      -- Outer redex: (λx.x) applied to ((λy.y) z)
      -- Inner redex: (λy.y) z
      -- Should substitute first (outermost), giving ((λy.y) z)
      let innerRedex = App (Lam y (Var y)) (Var z)
          expr = App (Lam x (Var x)) innerRedex
      shouldAlphaEqJust (callByName expr)
                        (Just innerRedex)

    it "reduces in function position through multiple layers" $ do
      -- ((λx.λy.y) a) b
      -- First step: reduce (λx.λy.y) a → λy.y
      -- Result: (λy.y) b
      let expr = App (App (Lam x (Lam y (Var y))) (Var a)) (Var b)
      shouldAlphaEqJust (callByName expr)
                        (Just (App (Lam y (Var y)) (Var b)))
