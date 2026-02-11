module Main (main) where

import           System.Environment (lookupEnv)
import           Text.Read          (readMaybe)

import           Web.App            (runApp)

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  port <- case portEnv of
    Nothing -> pure 3000
    Just value ->
      case readMaybe value of
        Just parsed -> pure parsed
        Nothing ->
          ioError $
            userError $
              ("Invalid PORT value: " ++ value)
  runApp port
