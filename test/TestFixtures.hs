module TestFixtures where

import           Named
import           Test.Hspec (Expectation, expectationFailure, shouldBe)

-- Common variables
f, w, x, x', y, z, a, b, y', y''  :: Var
f = MkVar "f"
w = MkVar "w"
x = MkVar "x"
x' = MkVar "x'"
y = MkVar "y"
y' = MkVar "y'"
y'' = MkVar "y''"
z = MkVar "z"
a = MkVar "a"
b = MkVar "b"

-- * Combinators *

-- I = λx.x
identityI :: Expr
identityI = Lam x (Var x)

-- K = λx.λy.x (True / Constant)
trueK :: Expr
trueK = Lam x (Lam y (Var x))

-- KI = λx.λy.y (False)
falseKI :: Expr
falseKI = Lam x (Lam y (Var y))

-- S = λx.λy.λz.x z (y z)
combinatorS :: Expr
combinatorS = Lam x (Lam y (Lam z (App (App (Var x) (Var z)) (App (Var y) (Var z)))))

-- SKK = I (S K K reduces to Identity)
skkExpr :: Expr
skkExpr = App (App combinatorS trueK) trueK

-- Helpful assertions
shouldAlphaEq :: Expr -> Expr -> Expectation
shouldAlphaEq actual expected =
  alphaEq actual expected `shouldBe` True

shouldNotAlphaEq :: Expr -> Expr -> Expectation
shouldNotAlphaEq actual expected =
  alphaEq actual expected `shouldBe` False

shouldAlphaEqJust :: Maybe Expr -> Maybe Expr -> Expectation
shouldAlphaEqJust actual expected =
  case liftA2 shouldAlphaEq actual expected of
    Nothing -> expectationFailure "Expected Just _, got Nothing"
    Just r  -> r
