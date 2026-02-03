module ParserSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Data.Void       (Void)
import           Named           (Expr (..), Var (..))
import           Parser          (ParsedProgram (..), parseNoPrelude,
                                  parseWithPrelude, preludeEnvProgram)
import           Test.Hspec
import           TestFixtures
import           Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

spec :: Spec
spec = do
  describe "Parser.Valid" $ do
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
      expectParse "\\x. x" identityI

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
        (App identityI (Var a))

    it "parses more complex grouping examples" $ do
      -- f (a x) z  == ((f (a x)) z)
      expectParse "f (a x) z"
        (App (App (Var f)
                  (App (Var a) (Var x)))
             (Var z))

  describe "Parser.LetBlock (equational reference)" $ do
    it "inlines a simple let binding into the final expression" $ do
      let flipL = (Lam f (Lam a (Lam b (App (App (Var f) (Var b)) (Var a)))))
      expectParse
        (unlines
          [ "let id   = \\x. x"
          , "let flip = \\f. \\a. \\b. f b a"
          , "flip id x y"
          ])
        (App (App (App flipL identityI) (Var x)) (Var y))

    it "supports forward references (unordered lets), as long as acyclic" $ do
      expectParse
        (unlines
          [ "let b = a"
          , "let a = x"
          , "b"
          ])
        (Var x)

    it "allows comments and blank space between let bindings" $ do
      expectParse
        (unlines
          [ ""
          , "-- first comment"
          , "let a = \\x. x  -- identity"
          , ""
          , "let b = a y"
          , "-- done"
          , "b"
          ])
        (App identityI (Var y))

    it "accept NO let bindings" $ do
      expectParse
        (unlines
          [ "b"
          ])
        (Var b)

  describe "Parser.Prelude" $ do
    it "loads I from the prelude" $ do
      expectParseWithPrelude "I" identityI
      expectParseWithPrelude "id" identityI

    it "allows user let-bindings to shadow prelude names" $ do
      let userI = Lam z (App (Var z) (Var z))
      expectParseWithPrelude
        (unlines
          [ "let I = \\z. z z"
          , "I"
          ])
        userI

  describe "Parser.Env" $ do
    it "WithPrelude + let bindings: check all of them are returned on env" $ do
      let input =
            unlines
              [ "let a = \\x. x"
              , "let b = a"
              , "b"
              ]
      withParsedProgram (parseWithPrelude input) $ \p -> do
        let env = namings p
            preludeKeys =
              either (error . errorBundlePretty) M.keysSet preludeEnvProgram
            expectedKeys =
              preludeKeys `S.union` S.fromList [MkVar "a", MkVar "b"]

        M.keysSet env `shouldBe` expectedKeys

    it "WithoutPrelude + let bindings: should only have let bindings" $ do
      let input =
            unlines
              [ "let a = \\x. x"
              , "let b = a"
              , "b"
              ]
      withParsedProgram (parseNoPrelude input) $ \p -> do
        let env = namings p
        env `shouldHaveKeys` map MkVar ["a", "b"]
        M.size env `shouldBe` 2

    it "WithoutPrelude + NO let bindings: M.empty" $ do
      withParsedProgram (parseNoPrelude "x") $ \p ->
        namings p `shouldBe` M.empty

  describe "Parser.Lexeme" $ do
    it "accepts leading spaces at the beginning of the expression" $ do
      expectParse "   x" (Var x)

    it "accepts trailing spaces at the end of the expression" $ do
      expectParse "x   " (Var x)

    it "accepts spaces after \\\\ (lambda token), and also works without them" $ do
      let expected = Lam x (Var x)
      expectParse "\\x. x" expected
      expectParse "\\ x. x" expected

    it "accepts spaces after '.' and also works without spaces after '.'" $ do
      let expected = (Lam x (Var x))
      expectParse "\\x.x" expected
      expectParse "\\x. x" expected

    it "accepts spaces after '(' and also works without spaces after '('" $ do
      let expected = Var x
      expectParse "(x)" expected
      expectParse "( x)" expected
      expectParse "( x )" expected

    it "accepts spaces in common parenthesized lambda application patterns" $ do
      let expected = (App (Lam x (Var x)) (Var a))
      expectParse "(\\x.x) a" expected
      expectParse "( \\x. x ) a" expected

  describe "Parser.Invalid" $ do
    it "rejects duplicate let-binding names" $ do
      expectFail
        (unlines
          [ "let a = x"
          , "let a = y"
          , "a"
          ])

    it "MUST be formatted as: many (letBindings) + finalExpr" $ do
      expectFail
        (unlines
          [ "b"
          ,"let b = a"
          ])

    it "disallows no new lines between bindings" $ do
      expectFail
        (unlines
          [ "let a=\\x.xlet b=a y"
          , "b"
          ])

    it "rejects cyclic (recursive) let-bindings" $ do
      expectFail
        (unlines
          [ "let a = b"
          , "let b = a"
          , "a"
          ])

    it "MUST have final expression" $ do
      expectFail
        (unlines
          [ "let b = a"
          , "let a = x"
          ])

    it "rejects a lambda missing the dot" $ do
      expectFail "\\x x"

    it "rejects an identifier starting with a digit" $ do
      expectFail "1x"

    it "rejects using reserved word 'let' as a variable" $ do
      expectFail "\\x. let y"

    it "does not treat identifiers like 'letx' as the let keyword" $ do
      let letx = MkVar "letx"
      expectParse "letx" (Var letx)

withParsedProgram
  :: Either (ParseErrorBundle String Void) ParsedProgram
  -> (ParsedProgram -> Expectation)
  -> Expectation
withParsedProgram ep k =
  case ep of
    Left err -> expectationFailure (errorBundlePretty err)
    Right p  -> k p

shouldHaveKeys :: (Ord k, Show k, Show v) => M.Map k v -> [k] -> Expectation
shouldHaveKeys m = mapM_ (\k -> m `shouldSatisfy` M.member k)

expectParse :: String -> Expr -> Expectation
expectParse s expected =
  case parsedExpr <$> parseNoPrelude s of
    Left err     -> expectationFailure (errorBundlePretty err)
    Right actual -> actual `shouldBe` expected

expectParseWithPrelude :: String -> Expr -> Expectation
expectParseWithPrelude s expected =
  case parsedExpr <$> parseWithPrelude s of
    Left err     -> expectationFailure (errorBundlePretty err)
    Right actual -> actual `shouldBe` expected

expectFail :: String -> Expectation
expectFail s =
  case parsedExpr <$> parseNoPrelude s of
    Left _ -> pure ()
    Right actual -> expectationFailure ("Expected parse failure, but succeeded with: " ++ show actual)
