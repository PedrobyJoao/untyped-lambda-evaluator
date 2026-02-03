{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception             (SomeException)
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Map.Strict               as M
import qualified Data.Text.Lazy                as TL
import           Named                         (BetaReduction (..), ElapsedNs,
                                                Expr, Var, alphaEq,
                                                evalWithStatistics)
import           Network.Wai.Middleware.Static (Policy, hasPrefix,
                                                isNotAbsolute, noDots,
                                                staticPolicy, (>->))
import           Parser                        (ParsedProgram (..),
                                                parseNoPrelude,
                                                parseWithPrelude)
import           Text.Megaparsec               (errorBundlePretty)
import           Text.Printf                   (printf)
import           Web.Scotty                    (ActionM, catch, file, formParam,
                                                get, html, middleware, post,
                                                scotty)

main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy staticUnderPrefix

  get "/" $ do
    file "static/index.html"

  post "/eval" $ do
    exprTxt <- formParam "expr" :: ActionM TL.Text
    stratTxt <- formParam "strategy" :: ActionM TL.Text
    withPreludeTxt <- (formParam "withPrelude" :: ActionM TL.Text)
      `catch` onMissingPrelude

    let parseFn = if isChecked withPreludeTxt then parseWithPrelude else parseNoPrelude

    case parseStrategy stratTxt of
      Left stratErr ->
        html $ renderOutputOnly stratErr

      Right strategy -> case parseFn (TL.unpack exprTxt) of

          Left parseErr ->
            html $ renderOutputOnly (TL.pack (errorBundlePretty parseErr))

          Right parsedProgram -> do
            (result, _trace, elapsedNs) <- liftIO $ evalWithStatistics strategy (parsedExpr parsedProgram)
            let outputTxt = resolveOutputIfReferenced result (namings parsedProgram)
            html $ renderOutputAndStats outputTxt (renderStats elapsedNs)

-- ============
-- Process I/O
-- ============

resolveOutputIfReferenced :: Expr -> M.Map Var Expr -> TL.Text
resolveOutputIfReferenced result env = TL.pack $
  maybe (show result) show (referenceName result env)

referenceName :: Expr -> M.Map Var Expr -> Maybe Var
referenceName result env =
  M.foldrWithKey
    (\v rhs acc ->
      case acc of
        Just _  -> acc
        Nothing -> if alphaEq result rhs then Just v else Nothing
    )
    Nothing
    env

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
-- =============

staticUnderPrefix :: Policy
staticUnderPrefix =
  -- noDots and isNotAbsolute are set by default but just to make sure..
  noDots >-> isNotAbsolute >-> hasPrefix "static/"

-- =============
-- Rendering
-- =============

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

renderStats :: ElapsedNs -> TL.Text
renderStats ns =
  let ms :: Double
      ms = fromIntegral ns / 1e6
  in TL.pack (printf "Elapsed: %.3f ms (%d ns)" ms ns)

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
