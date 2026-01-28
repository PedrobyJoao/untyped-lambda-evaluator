module AlphaRenameSpec where

import           Named
import           Test.Hspec
import           TestFixtures

spec :: Spec
spec = do
  describe "substitute" $ do
    it "substitutes a matching variable" $ do
      shouldAlphaEq (substitute x (Var x)
                            (Var y))
                            (Var y)

    it "leaves a non-matching variable unchanged" $ do
      shouldAlphaEq (substitute x (Var y)
                            (Var z))
                            (Var y)

    it "substitutes in the left side of an application" $ do
      shouldAlphaEq (substitute x (App (Var x) (Var y))
                            (Var z))
                            (App (Var z) (Var y))

    it "substitutes in the right side of an application" $ do
      shouldAlphaEq (substitute x (App (Var y) (Var x))
                            (Var z))
                            (App (Var y) (Var z))

    it "substitutes in both sides of an application" $ do
      shouldAlphaEq (substitute x (App (Var x) (Var x))
                            (Var y))
                            (App (Var y) (Var y))

    it "does not substitute under a lambda that binds the same variable" $ do
      shouldAlphaEq (substitute x (Lam x (Var x))
                            (Var y))
                            (Lam x (Var x))

    it "substitutes under a lambda that binds a different variable" $ do
      shouldAlphaEq (substitute x (Lam y (Var x))
                            (Var z))
                            (Lam y (Var z))

    it "substitutes a complex expression" $ do
      let free = App (Var y) (Var z)
      shouldAlphaEq (substitute x (Var x)
                            free)
                            (free)

    it "avoids variable capture" $ do
      -- (λy. x)[x := y] should become (λz. y), not (λy. y)
      -- The bound variable y must be renamed to avoid capturing the free y
      let result = substitute x
                        (Lam y (Var x))
                        (Var y)

      shouldAlphaEq result (Lam z (Var y))

    it "avoids variable capture when the would-be fresh name already exists as an inner binder (regression)" $ do
      -- This is the scenario described by:
      --   \x. (\x_1. x)
      -- If alpha-renaming ever chose x_1 as the new name for the *outer* x,
      -- the occurrence of x would become captured by the *inner* binder.
      --
      -- We force that situation through substitution:
      --   (λx. λx_1. x y)[y := x]
      --
      -- Since x is free in the replacement argument (x), we must rename the outer binder x.
      -- The "first guess" from nextVar x is x_1, but x_1 occurs anywhere in the body (as an inner binder),
      -- so the implementation must skip it.
      let x1 = nextVar x
          body = Lam x (Lam x1 (App (Var x) (Var y)))
          result = substitute y body (Var x)

      -- We don't assert the exact fresh binder chosen; we assert the meaning/structure:
      -- the first argument in the App must refer to the *outer* binder (DBBound 1),
      -- not the inner binder (DBBound 0).
      shouldAlphaEq result (Lam z (Lam x1 (App (Var z) (Var x))))

  describe "alphaRename" $ do
    it "renames the bound variable in a simple lambda" $ do
      let x1 = nextVar x
      alphaRename x1 (Lam x (Var x)) `shouldBe` Lam x1 (Var x1)

    it "leaves non-lambda expressions unchanged" $ do
      let x1 = nextVar x
      alphaRename x1 (Var x) `shouldBe` Var x
      alphaRename x1 (App (Var x) (Var y)) `shouldBe` App (Var x) (Var y)

    it "only renames occurrences of the bound variable" $ do
      -- λx. x y -> λx_1. x_1 y
      let x1 = nextVar x
      alphaRename x1 (Lam x (App (Var x) (Var y)))
        `shouldBe` Lam x1 (App (Var x1) (Var y))

    it "does not rename free variables" $ do
      -- λx. y -> λx_1. y
      let x1 = nextVar x
      alphaRename x1 (Lam x (Var y)) `shouldBe` Lam x1 (Var y)

    it "does not rename variables bound by inner lambdas" $ do
      -- λx. (λx. x) -> λx_1. (λx. x)
      let x1 = nextVar x
      alphaRename x1 (Lam x (Lam x (Var x)))
        `shouldBe` Lam x1 (Lam x (Var x))

    it "renames in nested applications" $ do
      -- λx. x (x z) -> λx_1. x_1 (x_1 z)
      let x1 = nextVar x
      alphaRename x1 (Lam x (App (Var x) (App (Var x) (Var z))))
        `shouldBe` Lam x1 (App (Var x1) (App (Var x1) (Var z)))

    it "does not capture due to choosing a name that already occurs as an inner binder" $ do
      -- \x. (\x_1. x)
      -- If we (incorrectly) rename outer x -> x_1, the x becomes captured by the inner binder.
      -- fetchFreshVar must therefore NOT choose x_1, and should choose x_2.
      let x1 = MkVar "x_1"
          x2 = MkVar "x_2"
          lam = Lam x (Lam x1 (Var x))
          chosen = fetchFreshVar x (Lam x1 (Var x)) (Var y)

      chosen `shouldBe` x2
      alphaRename chosen lam `shouldBe` Lam x2 (Lam x1 (Var x2))

  describe "isFreeIn" $ do
    it "returns True for a matching variable" $ do
      isFreeIn x (Var x) `shouldBe` True

    it "returns False for a non-matching variable" $ do
      isFreeIn x (Var y) `shouldBe` False

    it "returns True when variable appears free in left side of application" $ do
      isFreeIn x (App (Var x) (Var y)) `shouldBe` True

    it "returns True when variable appears free in right side of application" $ do
      isFreeIn x (App (Var y) (Var x)) `shouldBe` True

    it "returns False when variable does not appear in application" $ do
      isFreeIn x (App (Var y) (Var z)) `shouldBe` False

    it "returns False when variable is bound by lambda" $ do
      isFreeIn x (Lam x (Var x)) `shouldBe` False

    it "returns True when variable appears free under a different binder" $ do
      isFreeIn x (Lam y (Var x)) `shouldBe` True

    it "returns False when variable is shadowed by inner lambda" $ do
      isFreeIn x (Lam y (Lam x (Var x))) `shouldBe` False

    it "returns True when free in complex nested expression" $ do
      -- λy. (λz. x z) — x is free
      isFreeIn x (Lam y (Lam z (App (Var x) (Var z)))) `shouldBe` True

  describe "occursAnywhere" $ do
    it "returns True when the variable appears as a free variable" $ do
      occursAnywhere x (Var x) `shouldBe` True

    it "returns False when the variable does not appear" $ do
      occursAnywhere x (Var y) `shouldBe` False

    it "returns True when the variable appears in an application (left)" $ do
      occursAnywhere x (App (Var x) (Var y)) `shouldBe` True

    it "returns True when the variable appears in an application (right)" $ do
      occursAnywhere x (App (Var y) (Var x)) `shouldBe` True

    it "returns True when the variable appears as a binder" $ do
      occursAnywhere x (Lam x (Var y)) `shouldBe` True

    it "returns True when the variable appears under a lambda with a different binder" $ do
      occursAnywhere x (Lam y (App (Var x) (Var y))) `shouldBe` True

  describe "nextVar" $ do
    it "adds _1 when there is no numeric suffix" $ do
      nextVar (MkVar "x") `shouldBe` MkVar "x_1"

    it "increments an existing numeric suffix" $ do
      nextVar (MkVar "x_1") `shouldBe` MkVar "x_2"

    it "increments multi-digit numeric suffixes" $ do
      nextVar (MkVar "x_9") `shouldBe` MkVar "x_10"

    it "increments numeric suffixes when the base contains underscores" $ do
      nextVar (MkVar "a_b_1") `shouldBe` MkVar "a_b_2"
