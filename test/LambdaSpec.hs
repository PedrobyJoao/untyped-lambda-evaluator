module LambdaSpec where

import           Lambda
import           Test.Hspec

spec :: Spec
spec = do
  describe "substitute" $ do
    let x = MkVar "x"
        y = MkVar "y"
        z = MkVar "z"

    it "substitutes a matching variable" $ do
      substitute x (Var x) (Var y) `shouldBe` Var y

    it "leaves a non-matching variable unchanged" $ do
      substitute x (Var y) (Var z) `shouldBe` Var y

    it "substitutes in the left side of an application" $ do
      substitute x (App (Var x) (Var y)) (Var z)
        `shouldBe` App (Var z) (Var y)

    it "substitutes in the right side of an application" $ do
      substitute x (App (Var y) (Var x)) (Var z)
        `shouldBe` App (Var y) (Var z)

    it "substitutes in both sides of an application" $ do
      substitute x (App (Var x) (Var x)) (Var y)
        `shouldBe` App (Var y) (Var y)

    it "does not substitute under a lambda that binds the same variable" $ do
      substitute x (Lam x (Var x)) (Var y)
        `shouldBe` Lam x (Var x)

    it "substitutes under a lambda that binds a different variable" $ do
      substitute x (Lam y (Var x)) (Var z)
        `shouldBe` Lam y (Var z)

    it "substitutes a complex expression" $ do
      let free = App (Var y) (Var z)
      substitute x (Var x) free `shouldBe` free

    it "avoids variable capture" $ do
      -- (λy. x)[x := y] should become (λz. y), not (λy. y)
      -- The bound variable y must be renamed to avoid capturing the free y
      let result = substitute x (Lam y (Var x)) (Var y)
          Lam freshVar body = result
      -- The body should be the substituted free variable y, not the bound variable
      body `shouldBe` Var y
      -- The fresh variable should be different from y to avoid capture
      freshVar `shouldNotBe` y

    it "avoids variable capture when naive renaming would still capture" $ do
      -- (λy. (λy'. x))[x := y'] should become (λy. (λy''. y'))
      -- The inner binder y' must be renamed because y' appears free in the replacement
      -- The outer binder y does NOT need renaming (y is not free in y')
      let y' = MkVar "y'"
          y'' = MkVar "y''"
          -- body: λy. (λy'. x)
          body = Lam y (Lam y' (Var x))
          -- free expression: y'
          free = Var y'
          result = substitute x body free
      -- Should be: λy. (λy''. y')
      result `shouldBe` Lam y (Lam y'' (Var y'))

  describe "alphaRename" $ do
    let x = MkVar "x"
        x' = MkVar "x'"
        y = MkVar "y"
        z = MkVar "z"

    it "renames the bound variable in a simple lambda" $ do
      alphaRename (Lam x (Var x)) `shouldBe` Lam x' (Var x')

    it "leaves non-lambda expressions unchanged" $ do
      alphaRename (Var x) `shouldBe` Var x
      alphaRename (App (Var x) (Var y)) `shouldBe` App (Var x) (Var y)

    it "only renames occurrences of the bound variable" $ do
      -- λx. x y -> λx'. x' y
      alphaRename (Lam x (App (Var x) (Var y)))
        `shouldBe` Lam x' (App (Var x') (Var y))

    it "does not rename free variables" $ do
      -- λx. y -> λx'. y
      alphaRename (Lam x (Var y)) `shouldBe` Lam x' (Var y)

    it "does not rename variables bound by inner lambdas" $ do
      -- λx. (λx. x) -> λx'. (λx. x)
      alphaRename (Lam x (Lam x (Var x)))
        `shouldBe` Lam x' (Lam x (Var x))

    it "renames in nested applications" $ do
      -- λx. x (x z) -> λx'. x' (x' z)
      alphaRename (Lam x (App (Var x) (App (Var x) (Var z))))
        `shouldBe` Lam x' (App (Var x') (App (Var x') (Var z)))

    it "handles lambda with different bound variable in body" $ do
      -- λx. (λy. x y) -> λx'. (λy. x' y)
      alphaRename (Lam x (Lam y (App (Var x) (Var y))))
        `shouldBe` Lam x' (Lam y (App (Var x') (Var y)))

  describe "appearsFreeIn" $ do
    let x = MkVar "x"
        y = MkVar "y"
        z = MkVar "z"

    it "returns True for a matching variable" $ do
      appearsFreeIn x (Var x) `shouldBe` True

    it "returns False for a non-matching variable" $ do
      appearsFreeIn x (Var y) `shouldBe` False

    it "returns True when variable appears free in left side of application" $ do
      appearsFreeIn x (App (Var x) (Var y)) `shouldBe` True

    it "returns True when variable appears free in right side of application" $ do
      appearsFreeIn x (App (Var y) (Var x)) `shouldBe` True

    it "returns False when variable does not appear in application" $ do
      appearsFreeIn x (App (Var y) (Var z)) `shouldBe` False

    it "returns False when variable is bound by lambda" $ do
      appearsFreeIn x (Lam x (Var x)) `shouldBe` False

    it "returns True when variable appears free under a different binder" $ do
      appearsFreeIn x (Lam y (Var x)) `shouldBe` True

    it "returns False when variable is shadowed by inner lambda" $ do
      appearsFreeIn x (Lam y (Lam x (Var x))) `shouldBe` False

    it "returns True when free in complex nested expression" $ do
      -- λy. (λz. x z) — x is free
      appearsFreeIn x (Lam y (Lam z (App (Var x) (Var z)))) `shouldBe` True
