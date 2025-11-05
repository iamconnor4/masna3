module Masna3.Jobs.Poller where

import Data.Aeson
import Data.Poolboy
import Data.Text qualified as Text
import Effectful
import Effectful.Concurrent
import Effectful.Log
import Effectful.PostgreSQL
import Log qualified

import Masna3.Jobs.Job
import Masna3.Jobs.Worker

data PollerConfig m = PollerConfig
  { queueName :: Text
  -- ^ The queue to monitor
  , interval :: Int
  -- ^ In µ-seconds.
  , poolSettings :: PoolboySettings m
  }

mkPollerConfig :: Log :> es => Text -> PollerConfig (Eff es)
mkPollerConfig queueName =
  let poolSettings =
        PoolboySettings
          { workersCount = CapabilitiesWCS
          , workQueueName = Text.unpack queueName
          , logger = \command ->
              Log.logTrace
                "pool-worker"
                $ object
                  [ "command" .= show command
                  , "queue" .= queueName
                  ]
          }
   in PollerConfig
        { queueName
        , interval = 30000 -- 30ms
        , poolSettings
        }

monitorQueue
  :: (Concurrent :> es, FromJSON job, IOE :> es, Log :> es, WithConnection :> es)
  => PollerConfig (Eff es)
  -> WorkerConfig (Eff es) job
  -> Eff es ()
monitorQueue pollerConfig workerConfig = withPoolboy pollerConfig.poolSettings waitingStopFinishWorkers $ \workQueue ->
  loop workQueue
  where
    loop workQueue = do
      mJob <- readJob pollerConfig.queueName
      case mJob of
        Just job -> do
          Log.logInfo "Dispatching job" $
            object
              [ "job_id" .= job.id
              , "job_message" .= job.message
              , "queue_name" .= pollerConfig.queueName
              ]
          enqueue workQueue (runWorker workerConfig job)
        Nothing -> do
          threadDelay pollerConfig.interval
          loop workQueue
