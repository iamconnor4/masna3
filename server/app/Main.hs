module Main where

import BackgroundJobs.Poller qualified as Poller
import BackgroundJobs.Queue qualified as Queue
import BackgroundJobs.Worker (WorkerConfig (..))
import Data.Aeson
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL.Connection
import Effectful.Reader.Static
import Effectful.Time
import Log.Backend.StandardOutput qualified as Log

import Masna3.Server (runMasna3)
import Masna3.Server.Database
import Masna3.Server.Environment
import Masna3.Server.Jobs.Types
import Masna3.Server.Model.File.Query qualified as Query

main :: IO ()
main = runEff . runConcurrent $ do
  env <- getMasna3Env
  runReader env $
    Log.withStdOutLogger $ \logger -> do
      Log.runLog "masna3-jobs" logger Log.defaultLogLevel $
        runTime $
          withAsync startJobs $ \_ ->
            runMasna3
              logger
              env

startJobs
  :: ( Concurrent :> es
     , IOE :> es
     , Log :> es
     , Reader Masna3Env :> es
     , Time :> es
     )
  => Eff es ()
startJobs = withPool $ do
  let pollerConfig = Poller.mkPollerConfig "masna3_jobs"
  let workerConfig :: (IOE :> es, Log :> es, Time :> es, WithConnection :> es) => WorkerConfig (Eff es) Masna3Job
      workerConfig =
        WorkerConfig
          { queueName = "testqueue"
          , onException = \_ _ -> error "Caught exception"
          , process = \case
              ListExpiredFiles -> do
                files <- Query.listExpiredFiles
                Log.logInfo "Expired files" $
                  object ["amount" .= length files]
          }
  Queue.createQueue "masna3_jobs"
  Poller.monitorQueue pollerConfig workerConfig
