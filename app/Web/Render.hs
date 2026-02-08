{-# LANGUAGE OverloadedStrings #-}

module Web.Render where

import           Data.List      (intersperse)
import qualified Data.Text.Lazy as TL
import           Named          (ElapsedNs, EvalStopReason (..), Trace)
import           Text.Printf    (printf)

-- ============
-- HTMX: we're using htmx so we have to render the html
-- from the server
-- =============

-- Public renderers (organized by caller needs)

renderOutputOnly :: TL.Text -> TL.Text
renderOutputOnly outputText =
  renderOutputSection outputText

renderOutputAndStatistics
  :: TL.Text
  -> ElapsedNs
  -> Int
  -> EvalStopReason
  -> TL.Text
renderOutputAndStatistics outputText elapsedNs stepsCount stopReason=
  mconcat
    [ renderOutputSection outputText
    , renderStatisticsSection elapsedNs stepsCount stopReason
    ]

renderOutputAndStatisticsAndSteps
  :: TL.Text
  -> ElapsedNs
  -> Int
  -> EvalStopReason
  -> Trace
  -> TL.Text
renderOutputAndStatisticsAndSteps outputText elapsedNs stepsCount stopReason _trace =
  mconcat
    [ renderOutputSection outputText
    , renderStatisticsSection elapsedNs stepsCount stopReason
    , renderStepsSection _trace
    ]

-- Sections

renderOutputSection :: TL.Text -> TL.Text
renderOutputSection outputText =
  mconcat
    [ "<section id=\"outputSection\">"
    , "<p class=\"result-label\">Output:</p>"
    , "<pre class=\"result-pre\" id=\"outputPre\">"
    , escapeHtml outputText
    , "</pre>"
    , "</section>"
    ]

renderStatisticsSection :: ElapsedNs -> Int -> EvalStopReason -> TL.Text
renderStatisticsSection elapsedNs stepsCount stopReason =
  let lines' =
        [ renderElapsedLine elapsedNs
        , renderBetaStepsLine stepsCount
        , "Termination Reason: " <> TL.pack (show stopReason)
        ]
  in mconcat
      [ "<section id=\"statsSection\">"
      , "<p class=\"result-label\">Statistics:</p>"
      , "<div class=\"result-box\" id=\"statsBox\">"
      , "<div id=\"statsContent\">"
      , renderLinesWithBreaks lines'
      , "</div>"
      , "</div>"
      , "</section>"
      ]

renderStepsSection :: Trace -> TL.Text
renderStepsSection trace =
  mconcat
    [ "<section id=\"stepsSection\">"
    , "<p class=\"result-label\">β-steps:</p>"
    , "<pre class=\"result-pre\" id=\"stepsPre\">"
    , escapeHtml (TL.pack (show trace))
    , "</pre>"
    , "</section>"
    ]

-- Statistics helpers

renderElapsedLine :: ElapsedNs -> TL.Text
renderElapsedLine ns =
  let ms :: Double
      ms = fromIntegral ns / 1e6
  in TL.pack (printf "Elapsed: %.3f ms (%d ns)" ms ns)

renderBetaStepsLine :: Int -> TL.Text
renderBetaStepsLine stepsCount
  | stepsCount <= 0 = "β-Reductions: No β-reductions, already in normal form (or WHNF if call-by-name)"
  | otherwise = "β-steps: " <> TL.pack (show stepsCount)

renderLinesWithBreaks :: [TL.Text] -> TL.Text
renderLinesWithBreaks =
  mconcat . intersperse "<br/>" . map escapeHtml

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

-- just an easter egg
renderCallByValue :: TL.Text
renderCallByValue = renderOutputAndStatistics
                  msg
                  0
                  0
                  NoMoreReductions
    where msg = TL.unlines [
              "not implemented yet. sorry.",
              "",
              "--",
              "",
              "breathe. be aware of the uniqueness of this very moment.",
              "",
              "feel the sensations of the air entering your nostrils.",
              "",
              "enjoy our ephemeral conscious experience."
              ]
