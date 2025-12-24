module NamedCallByNameSpec where

import           Named
import           Test.Hspec

spec :: Spec
spec = do
  describe "eval (call by name)" $ do
    let x = MkVar "x"
        y = MkVar "y"
        z = MkVar "z"

    -- Identity: λx.x
    let identity = Lam x (Var x)

    -- Const: λx.λy.x
    let const' = Lam x (Lam y (Var x))

    it "returns a variable as-is" $ do
      eval CallByName (Var x) `shouldBe` Var x

    it "returns a lambda as-is" $ do
      eval CallByName identity `shouldBe` identity

    it "applies identity to a variable" $ do
      -- (λx.x) y → y
      eval CallByName (App identity (Var y)) `shouldBe` Var y

    it "applies identity to identity" $ do
      -- (λx.x) (λx.x) → λx.x
      eval CallByName (App identity identity) `shouldBe` identity

    it "applies const to two arguments" $ do
      -- (λx.λy.x) a b → a
      let a = Var (MkVar "a")
          b = Var (MkVar "b")
      eval CallByName (App (App const' a) b) `shouldBe` a

    it "does not reduce under lambdas" $ do
      -- λx.(λy.y) x should NOT reduce to λx.x
      -- call by name stops at the outer lambda
      let inner = App (Lam y (Var y)) (Var x)
          expr = Lam x inner
      eval CallByName expr `shouldBe` expr

    it "does not evaluate arguments before application" $ do
      -- (λx.λy.y) ((λx.x)(λx.x)) → λy.y
      -- The argument is never needed, so it's not evaluated
      let omega = App identity identity
          expr = App (Lam x (Lam y (Var y))) omega
      eval CallByName expr `shouldBe` Lam y (Var y)

    it "handles nested applications" $ do
      -- ((λx.x) (λy.y)) z → z
      eval CallByName (App (App identity (Lam y (Var y))) (Var z)) `shouldBe` Var z

    it "handles self-application on simple terms" $ do
      -- (λx.x x) (λy.y) → (λy.y) (λy.y) → λy.y
      let selfApp = Lam x (App (Var x) (Var x))
      eval CallByName (App selfApp identity) `shouldBe` identity

    it "avoids variable capture during evaluation" $ do
      -- (λx.λy.x) y → λy'.y
      -- The inner binder must be renamed to avoid capturing the free y
      let expr = App (Lam x (Lam y (Var x))) (Var y)
          result = eval CallByName expr
          Lam boundVar body = result
      body `shouldBe` Var y
      boundVar `shouldNotBe` y

  describe "callByName (single step reduction)" $ do
    let x = MkVar "x"
        y = MkVar "y"
        z = MkVar "z"
        w = MkVar "w"

    it "reduces leftmost redex first" $ do
      -- (λx.x) y ((λz.z) w)
      -- Two redexes: (λx.x) y and (λz.z) w
      -- Should reduce left one first: y ((λz.z) w)
      let leftRedex = App (Lam x (Var x)) (Var y)
          rightRedex = App (Lam z (Var z)) (Var w)
          expr = App leftRedex rightRedex
      callByName expr `shouldBe` Just (App (Var y) rightRedex)

    it "reduces outermost redex before inner redex" $ do
      -- (λx.x) ((λy.y) z)
      -- Outer redex: (λx.x) applied to ((λy.y) z)
      -- Inner redex: (λy.y) z
      -- Should substitute first (outermost), giving ((λy.y) z)
      let innerRedex = App (Lam y (Var y)) (Var z)
          expr = App (Lam x (Var x)) innerRedex
      callByName expr `shouldBe` Just innerRedex

    it "reduces in function position through multiple layers" $ do
      -- ((λx.λy.y) a) b
      -- First step: reduce (λx.λy.y) a → λy.y
      -- Result: (λy.y) b
      let a = MkVar "a"
          b = MkVar "b"
          expr = App (App (Lam x (Lam y (Var y))) (Var a)) (Var b)
      callByName expr `shouldBe` Just (App (Lam y (Var y)) (Var b))
