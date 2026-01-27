module AlphaEqSpec where

import           Named
import           Test.Hspec
import           TestFixtures

spec :: Spec
spec = do
  describe "toDB" $ do
    it "converts a bound variable to DBBound 0" $ do
      toDB (Lam x (Var x)) `shouldBe` DBLam (DBBound 0)

    it "keeps a free variable as DBFree" $ do
      toDB (Var y) `shouldBe` DBFree y

    it "uses correct indices for nested binders (nearest is 0)" $ do
      -- \x.\y. x y
      toDB (Lam x (Lam y (App (Var x) (Var y))))
        `shouldBe` DBLam (DBLam (DBApp (DBBound 1) (DBBound 0)))

    it "treats a shadowed variable as the nearest binder" $ do
      -- \x.\x. x
      toDB (Lam x (Lam x (Var x)))
        `shouldBe` DBLam (DBLam (DBBound 0))

  describe "alphaEq" $ do
    it "considers \\x.x and \\y.y alpha-equivalent" $ do
      shouldAlphaEq (Lam x (Var x))
                    (Lam y (Var y))

    it "considers \\x.\\y.x alpha-equivalent to \\a.\\b.a" $ do
      shouldAlphaEq (Lam x (Lam y (Var x)))
                    (Lam a (Lam b (Var a)))

    it "does not equate different free variables" $ do
      shouldNotAlphaEq (Lam x (Var y))
                       (Lam z (Var z))

    it "does not equate a bound variable with a free one" $ do
      shouldNotAlphaEq (Lam x (Var x))
                       (Var x)

    it "requires the same application structure" $ do
      shouldNotAlphaEq (Lam x (App (Var x) (Var x)))
                       (Lam y (Var y))

    it "treats terms with the same free variable as alpha-equivalent" $ do
      -- \x.y  ~=  \z.y   (y is free in both)
      shouldAlphaEq (Lam x (Var y))
                    (Lam z (Var y))

    it "does not consider different free variables alpha-equivalent even if binders match" $ do
      -- \x.a  /=  \x.b
      shouldNotAlphaEq (Lam x (Var a))
                       (Lam x (Var b))

    it "handles shadowing: \\x.\\x.x is alpha-equivalent to \\a.\\b.b" $ do
      -- In \x.\x.x the inner binder is the one referenced.
      shouldAlphaEq (Lam x (Lam x (Var x)))
                    (Lam a (Lam b (Var b)))

    it "distinguishes which binder a variable refers to (binder position matters)" $ do
      -- \x.\y.x y  /=  \a.\b.b a
      shouldNotAlphaEq (Lam x (Lam y (App (Var x) (Var y))))
                       (Lam a (Lam b (App (Var b) (Var a))))

    it "does not equate \\x.\\y.x with \\x.\\y.y" $ do
      shouldNotAlphaEq (Lam x (Lam y (Var x)))
                       (Lam x (Lam y (Var y)))

    it "considers alpha-equivalent when only the bound variable name changes under an application with a free var" $ do
      -- \x. x y  ~=  \z. z y
      shouldAlphaEq (Lam x (App (Var x) (Var y)))
                    (Lam z (App (Var z) (Var y)))

    it "does not equate applications with swapped free variables" $ do
      shouldNotAlphaEq (App (Var x) (Var y))
                       (App (Var y) (Var x))

    it "does not equate a lambda with an application (different outer constructor)" $ do
      shouldNotAlphaEq (Lam x (Var x))
                       (App (Var x) (Var x))

    it "does not equate lambdas with different arity/structure" $ do
      -- \x.\y.x  /=  \x.x
      shouldNotAlphaEq (Lam x (Lam y (Var x)))
                       (Lam x (Var x))
