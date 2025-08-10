module Main where

import Control.Concurrent.MVar.Strict qualified as IOMVar
import Effectful
import Effectful.Concurrent.Async
import Effectful.Concurrent.MVar.Strict qualified as MVar
import Effectful.Log qualified as Log
import Effectful.PostgreSQL
import Effectful.Reader.Static qualified as Reader
import Log.Backend.StandardOutput qualified as Log
import Network.Wai.Handler.Warp
import Network.Wai.Log qualified as WaiLog
import System.IO
import Test.Tasty

import Masna3.Server
import Masna3.Server.Database
import Masna3.Server.Environment
import Masna3.Test.File qualified as File
import Masna3.Test.Utils

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  env <- runEff getMasna3Env
  runEff $ Reader.runReader env $ withPool cleanUp
  tests <- traverse (`runTestEff` env) specs
  semaphore <- IOMVar.newEmptyMVar'
  let server = Log.withStdOutLogger $ \logger -> do
        loggingMiddleware <- Log.runLog "masna3-test-server" logger Log.defaultLogLevel WaiLog.mkLogMiddleware
        let warpSettings =
              defaultSettings
                & setPort (fromIntegral env.httpPort)
                & setBeforeMainLoop (IOMVar.putMVar' semaphore ())
        liftIO
          $ runSettings warpSettings
          $ loggingMiddleware
            . const
          $ makeServer logger env
  runEff $ runConcurrent $ withAsync server $ \_ -> do
    MVar.readMVar' semaphore
    liftIO $
      defaultMain $
        testGroup "Masna3 Tests" tests

specs :: [TestEff TestTree]
specs =
  [ File.spec
  ]

cleanUp :: (IOE :> es, WithConnection :> es) => Eff es ()
cleanUp = do
  void $ execute "DELETE FROM files" ()
  void $ execute "DELETE FROM owners" ()
