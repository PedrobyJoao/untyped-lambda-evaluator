{-# LANGUAGE OverloadedStrings #-}

module Web.Render where

import qualified Data.Text.Lazy as TL
import           Named          (ElapsedNs)
import           Text.Printf    (printf)

-- ============
-- HTMX: we're using htmx so we have to render the html
-- from the server
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
