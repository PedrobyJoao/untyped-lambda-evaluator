{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Text.Lazy                as TL
import           Named                         (BetaReduction (..), ElapsedNs,
                                                evalWithStatistics)
import           Network.Wai.Middleware.Static (Policy, hasPrefix,
                                                isNotAbsolute, noDots,
                                                staticPolicy, (>->))
import           Parser                        (parseStr)
import           Text.Megaparsec               (errorBundlePretty)
import           Text.Printf                   (printf)
import           Web.Scotty                    (ActionM, file, formParam, get,
                                                html, middleware, post, scotty)

main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy staticUnderPrefix

  get "/" $ do
    file "static/index.html"

  post "/eval" $ do
    exprTxt <- formParam "expr" :: ActionM TL.Text
    stratTxt <- formParam "strategy" :: ActionM TL.Text

    case parseStrategy stratTxt of
      Left stratErr ->
        html $ renderOutputOnly stratErr
      Right strategy ->
        case parseStr (TL.unpack exprTxt) of
          Left parseErr ->
            html $ renderOutputOnly (TL.pack (errorBundlePretty parseErr))
          Right parsedExpr -> do
            (result, _trace, elapsedNs) <- liftIO $ evalWithStatistics strategy parsedExpr
            html $
              renderOutputAndStats
                (TL.pack (show result))
                (renderStats elapsedNs)

renderStats :: ElapsedNs -> TL.Text
renderStats ns =
  let ms :: Double
      ms = fromIntegral ns / 1e6
  in TL.pack (printf "Elapsed: %.3f ms (%d ns)" ms ns)

parseStrategy :: TL.Text -> Either TL.Text BetaReduction
parseStrategy t =
  case TL.toLower t of
    "normal"      -> Right NormalOrder
    "cbn"         -> Right CallByName
    "applicative" -> Right Applicative
    other         -> Left $ "Unknown strategy: " <> other

staticUnderPrefix :: Policy
staticUnderPrefix =
  -- noDots and isNotAbsolute are set by default but just to make sure..
  noDots >-> isNotAbsolute >-> hasPrefix "static/"

renderOutputOnly :: TL.Text -> TL.Text
renderOutputOnly outputText =
  mconcat
    [ "<section id=\"outputSection\">"
    , "<p class=\"result-label\">Output:</p>"
    , "<pre class=\"result-pre\" id=\"outputPre\">"
    , escapeHtml outputText
    , "</pre>"
    , "</section>"
    ]

renderOutputAndStats :: TL.Text -> TL.Text -> TL.Text
renderOutputAndStats outputText statsInner =
  mconcat
    [ "<section id=\"outputSection\">"
    , "<p class=\"result-label\">Output:</p>"
    , "<pre class=\"result-pre\" id=\"outputPre\">"
    , escapeHtml outputText
    , "</pre>"
    , "</section>"
    , "<section id=\"statsSection\">"
    , "<p class=\"result-label\">Statistics:</p>"
    , "<div class=\"result-box\" id=\"statsBox\">"
    , "<div id=\"statsContent\">"
    , escapeHtml statsInner
    , "</div>"
    , "</div>"
    , "</section>"
    ]

-- minimal HTML escaping to avoid breaking markup when embedding errors/results
escapeHtml :: TL.Text -> TL.Text
escapeHtml =
  TL.concatMap $ \c -> case c of
    '&'  -> "&amp;"
    '<'  -> "&lt;"
    '>'  -> "&gt;"
    '"'  -> "&quot;"
    '\'' -> "&#39;"
    _    -> TL.singleton c
