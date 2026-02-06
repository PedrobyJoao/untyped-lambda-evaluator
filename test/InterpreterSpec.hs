module InterpreterSpec (spec) where

import qualified Data.Text.Lazy  as TL
import           Interpreter     (WithPrelude, interpret)
import           Named           (BetaReduction (..), ElapsedNs,
                                  EvalResult (..), EvalStopReason (..),
                                  maxEvalSteps)
import           System.FilePath ((</>))
import           Test.Hspec

exIdentityLetApply = "identity_let_apply.lam"
exPreludeIdApplication = "prelude_id_application.lam"
exForwardRefResult = "forward_ref_result.lam"
exPreludePlusOneTwo = "prelude_plus_one_two.lam"
exLocalBooleanIf = "local_boolean_if.lam"
exOmegaShortCircuit = "omega_short_circuit.lam"
exShadowPreludeId = "shadow_prelude_id.lam"
exSequenceOfPrimes = "sequence_of_primes.lam"

spec :: Spec
spec = do
  describe "interpret (end-to-end)" $ do
    describe "Identity and basic application" $ do
      let out = "y"
      mapM_
        ( \strategy ->
            it (show strategy <> " reduces to y") $
              expectOutput strategy False exIdentityLetApply out
        )
        [Applicative, NormalOrder, CallByName]

    describe "Prelude toggle" $ do
      let outWithPrelude = "a"
      it "uses prelude when enabled" $
        expectOutput NormalOrder True exPreludeIdApplication outWithPrelude

      let outWithoutPrelude = "id a"
      it "treats prelude names as free variables when disabled" $
        expectOutput NormalOrder False exPreludeIdApplication outWithoutPrelude

    describe "Let bindings and output name resolution" $ do
      let out = "zeta"
      it "resolves forward references and returns the binding name" $
        expectOutput NormalOrder False exForwardRefResult out

    describe "Local booleans" $ do
      let out = "yes"
      it "reduces if true yes no to yes" $
        expectOutput NormalOrder False exLocalBooleanIf out

    describe "Prelude church numerals" $ do
      let out = "three"
      mapM_
        ( \strategy ->
            it (show strategy <> " reduces plus one two to three") $
              expectOutput strategy True exPreludePlusOneTwo out
        )
        [Applicative, NormalOrder]

    describe "Prelude shadowing" $ do
      let out = "z z"
      it "prefers user-defined id over the prelude" $
        expectOutput NormalOrder True exShadowPreludeId out

    describe "Short-circuiting non-termination" $ do
      let out = "ok"
      it "does not evaluate omega under call-by-name" $
        expectOutput CallByName False exOmegaShortCircuit out
      it "normal order (lazy) fully normalizes without trying to reduce omega" $
        expectOutput NormalOrder False exOmegaShortCircuit out

    describe "Max steps" $ do
      it "applicative does not guarantee termination even when the lam expr has a normal form" $
        expectStopReason
          Applicative
          False
          exOmegaShortCircuit
          (MaxNumberOfSteps maxEvalSteps)
      it "stops at the step limit for a non-terminating program" $
        expectStopReason
          NormalOrder
          False
          exSequenceOfPrimes
          (MaxNumberOfSteps maxEvalSteps)

readExample :: FilePath -> IO String
readExample name = readFile ("examples" </> name)

interpretExample ::
  BetaReduction ->
  WithPrelude ->
  FilePath ->
  IO (Either TL.Text (TL.Text, EvalResult, ElapsedNs))
interpretExample strategy withPrelude name = do
  program <- readExample name
  interpret strategy withPrelude program

withInterpret ::
  BetaReduction ->
  WithPrelude ->
  FilePath ->
  (TL.Text -> EvalResult -> Expectation) ->
  Expectation
withInterpret strategy withPrelude exampleName assert = do
  result <- interpretExample strategy withPrelude exampleName
  case result of
    Left err                      -> expectationFailure (TL.unpack err)
    Right (outputTxt, evalRes, _) -> assert outputTxt evalRes

expectOutput ::
  BetaReduction ->
  WithPrelude ->
  FilePath ->
  String ->
  Expectation
expectOutput strategy withPrelude exampleName expected =
  withInterpret strategy withPrelude exampleName $ \outputTxt evalRes -> do
    outputTxt `shouldBe` TL.pack expected
    stopReason evalRes `shouldBe` NoMoreReductions

expectStopReason ::
  BetaReduction ->
  WithPrelude ->
  FilePath ->
  EvalStopReason ->
  Expectation
expectStopReason strategy withPrelude exampleName expected =
  withInterpret strategy withPrelude exampleName $ \_ evalRes ->
    stopReason evalRes `shouldBe` expected
