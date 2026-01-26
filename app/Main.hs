{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.Lazy                as TL
import           Named                         (BetaReduction (..), eval)
import           Network.Wai.Middleware.Static (Policy, hasPrefix,
                                                isNotAbsolute, noDots,
                                                staticPolicy, (>->))
import           Parser                        (parseStr)
import           Text.Megaparsec               (errorBundlePretty)
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
        html $ renderOutputOnly ("<pre>" <> escapeHtml stratErr <> "</pre>")
      Right strategy ->
        case parseStr (TL.unpack exprTxt) of
          Left parseErr ->
            html $ renderOutputOnly ("<pre>" <> escapeHtml (TL.pack (errorBundlePretty parseErr)) <> "</pre>")
          Right parsedExpr -> do
            let result = eval strategy parsedExpr
            html $
              renderOutputAndStats
                ("<pre>" <> escapeHtml (TL.pack (show result)) <> "</pre>")
                ("<em>No statistics yet.</em>")

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
renderOutputOnly outputInner =
  mconcat
    [ "<section>"
    , "<h2>Output expression / Error</h2>"
    , "<article id=\"output\">"
    , "<div id=\"resultContent\">"
    , outputInner
    , "</div>"
    , "</article>"
    , "</section>"
    ]

renderOutputAndStats :: TL.Text -> TL.Text -> TL.Text
renderOutputAndStats outputInner statsInner =
  mconcat
    [ renderOutputOnly outputInner
    , "<section>"
    , "<h2>Dropdown statistics</h2>"
    , "<article id=\"stats\">"
    , "<div id=\"statsContent\">"
    , statsInner
    , "</div>"
    , "</article>"
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
