module Main where

import Control.Monad
import Effectful
import Effectful.PostgreSQL
import Effectful.Reader.Static qualified as Reader
import System.IO
import Test.Tasty

import Masna3.Server.Database
import Masna3.Server.Environment
import Masna3.Test.Utils

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  env <- runEff getMasna3Env
  runEff $ Reader.runReader env $ withPool cleanUp
  tests <- traverse (`runTestEff` env) specs
  defaultMain $
    testGroup "Masna3 Tests" tests

specs :: [TestEff TestTree]
specs = []

cleanUp :: (IOE :> es, WithConnection :> es) => Eff es ()
cleanUp = do
  void $ execute "DELETE FROM files" ()
  void $ execute "DELETE FROM owners" ()
