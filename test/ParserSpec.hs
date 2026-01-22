module ParserSpec (spec) where

import           Named      (Expr (..), Var (..))
import           Parser     (parseStr)
import           Test.Hspec

spec :: Spec
spec = do
  describe "Parser.parseStr" $ do
    let x = MkVar "x"
        y = MkVar "y"
        z = MkVar "z"
        f = MkVar "f"
        g = MkVar "g"
        a = MkVar "a"

    it "parses a variable" $ do
      expectParse "x" (Var x)

    it "parses identifiers with underscores and digits (not as first char)" $ do
      let x2  = MkVar "_x2"
          xy2 = MkVar "x_y2"
      expectParse "_x2" (Var x2)
      expectParse "x_y2" (Var xy2)

    it "parses application as left-associative chaining" $ do
      expectParse "f x y"
        (App (App (Var f) (Var x))
             (Var y))

    it "uses parentheses to override grouping" $ do
      expectParse "f (x y)"
        (App (Var f)
             (App (Var x) (Var y)))

      expectParse "(f x) y"
        (App (App (Var f) (Var x))
             (Var y))

    it "parses a lambda abstraction" $ do
      expectParse "\\x. x"
        (Lam x (Var x))

    it "parses a lambda whose body is an application" $ do
      expectParse "\\x. x y"
        (Lam x
             (App (Var x) (Var y)))

    it "parses nested lambdas" $ do
      expectParse "\\x. \\y. x"
        (Lam x
             (Lam y (Var x)))

    it "parses application where the function is a parenthesized lambda" $ do
      expectParse "(\\x. x) a"
        (App (Lam x (Var x))
             (Var a))

    it "parses more complex grouping examples" $ do
      -- f (g x) z  == ((f (g x)) z)
      expectParse "f (g x) z"
        (App (App (Var f)
                  (App (Var g) (Var x)))
             (Var z))

    it "rejects a lambda missing the dot" $ do
      expectFail "\\x x"

    it "rejects an identifier starting with a digit" $ do
      expectFail "1x"

expectParse :: String -> Expr -> Expectation
expectParse s expected =
  case parseStr s of
    Left err     -> expectationFailure (show err)
    Right actual -> actual `shouldBe` expected

expectFail :: String -> Expectation
expectFail s =
  case parseStr s of
    Left _ -> pure ()
    Right actual -> expectationFailure ("Expected parse failure, but succeeded with: " ++ show actual)
