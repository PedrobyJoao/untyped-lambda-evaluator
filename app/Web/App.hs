{-# LANGUAGE OverloadedStrings #-}

module Web.App
  ( runApp
  ) where

import           Control.Exception             (SomeException)
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Text.Lazy                as TL
import           Interpreter                   (interpret)
import           Named                         (BetaReduction (..))
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
  withPreludeTxt <- (formParam "withPrelude" :: ActionM TL.Text)
    `catch` onMissingPrelude

  let withPrelude = isChecked withPreludeTxt

  case parseStrategy stratTxt of
    Left stratErr ->
      html $ renderOutputOnly stratErr

    Right strategy -> do
      result <- liftIO $ interpret strategy withPrelude (TL.unpack exprTxt)
      case result of
        Left errTxt ->
          html $ renderOutputOnly errTxt
        Right (outputTxt, _, elapsedNs) ->
          html $ renderOutputAndStats outputTxt (renderStats elapsedNs)

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

onMissingPrelude :: SomeException -> ActionM TL.Text
onMissingPrelude _ = pure "off"

-- ============
-- Middleware
-- ============

staticUnderPrefix :: Policy
staticUnderPrefix =
  -- noDots and isNotAbsolute are set by default but just to make sure..
  noDots >-> isNotAbsolute >-> hasPrefix "static/"
