module Main where

import Effectful
import Log.Backend.StandardOutput qualified as Log

import Masna3.Server (runMasna3)
import Masna3.Server.Environment

main :: IO ()
main = runEff $ do
  env <- getMasna3Env
  Log.withStdOutLogger $ \logger ->
    runMasna3
      logger
      env
