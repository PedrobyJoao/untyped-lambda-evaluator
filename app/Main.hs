{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.Lazy                as TL
import           Network.Wai.Middleware.Static (Policy, addBase, hasPrefix,
                                                isNotAbsolute, noDots,
                                                staticPolicy, (>->))
import           Web.Scotty                    (ActionM, file, formParam, get,
                                                middleware, post, scotty, text)

main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy staticUnderPrefix

  get "/" $ do
    file "static/index.html"

  post "/eval" $ do
    expr <- formParam "expr" :: ActionM TL.Text
    strat <- formParam "strategy" :: ActionM TL.Text
    text $ "TODO eval; strategy=" <> strat <> "; expr=" <> expr

staticUnderPrefix :: Policy
staticUnderPrefix =
  -- noDots and isNotAbsolute are set by default but just to make sure..
  noDots >-> isNotAbsolute >-> hasPrefix "static/" >-> addBase "static"
