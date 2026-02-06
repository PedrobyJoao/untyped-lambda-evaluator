{-# LANGUAGE OverloadedStrings #-}

module Web.App
  ( runApp,
  )
where

import           Control.Exception             (SomeException)
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Text.Lazy                as TL
import           Interpreter                   (interpret)
import           Named                         (BetaReduction (..),
                                                EvalResult (..),
                                                EvalStopReason (NoMoreReductions),
                                                Trace (..))
import           Network.Wai.Middleware.Static (Policy, hasPrefix,
                                                isNotAbsolute, noDots,
                                                staticPolicy, (>->))
import           Web.Render
import           Web.Scotty                    (ActionM, ScottyM, catch, file,
                                                formParam, get, html,
                                                middleware, post, scotty)

runApp :: IO ()
runApp =
  scotty 3000 $ do
    middleware $ staticPolicy staticUnderPrefix
    routes

routes :: ScottyM ()
routes = do
  get "/" getHome
  post "/eval" postEval

getHome :: ActionM ()
getHome =
  file "static/index.html"

postEval :: ActionM ()
postEval = do
  exprTxt <- formParam "expr" :: ActionM TL.Text
  stratTxt <- formParam "strategy" :: ActionM TL.Text

  withPreludeTxt <-
    (formParam "withPrelude" :: ActionM TL.Text)
      `catch` onMissingCheckbox
  showStepsTxt <-
    (formParam "showSteps" :: ActionM TL.Text)
      `catch` onMissingCheckbox

  let withPrelude = isChecked withPreludeTxt
  let showSteps = isChecked showStepsTxt

  if TL.toLower (TL.strip stratTxt) == "jotapex"
    then html $ renderJotapex

    else case parseStrategy stratTxt of
      Left stratErr ->
        html $ renderOutputOnly stratErr
      Right strategy -> do
        result <- liftIO $ interpret strategy withPrelude (TL.unpack exprTxt)
        case result of
          Left errTxt ->
            html $ renderOutputOnly errTxt
          Right (outputTxt, evalRes, elapsedNs) -> do
            let Trace steps = evalTrace evalRes
            let stepsCount = length steps

            if showSteps && stepsCount > 0
              then
                html $
                  renderOutputAndStatisticsAndSteps
                    outputTxt
                    elapsedNs
                    stepsCount
                    (stopReason evalRes)
                    (evalTrace evalRes)
              else
                html $
                  renderOutputAndStatistics
                    outputTxt
                    elapsedNs
                    stepsCount
                    (stopReason evalRes)

-- ============
-- Process I/O
-- ============

parseStrategy :: TL.Text -> Either TL.Text BetaReduction
parseStrategy t =
  case TL.toLower t of
    "normal"      -> Right NormalOrder
    "cbn"         -> Right CallByName
    "applicative" -> Right Applicative
    other         -> Left $ "Unknown strategy: " <> other

isChecked :: TL.Text -> Bool
isChecked t = TL.toLower (TL.strip t) == "on"

onMissingCheckbox :: SomeException -> ActionM TL.Text
onMissingCheckbox _ = pure "off"

-- ============
-- Middleware
-- ============

staticUnderPrefix :: Policy
staticUnderPrefix =
  -- noDots and isNotAbsolute are set by default but just to make sure..
  noDots >-> isNotAbsolute >-> hasPrefix "static/"
