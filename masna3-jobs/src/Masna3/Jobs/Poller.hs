module Masna3.Jobs.Poller where

import Control.Concurrent
import Data.Poolboy
import Data.Text qualified as Text
import Effectful
import Effectful.Log
import Effectful.PostgreSQL
import Log qualified

import Masna3.Jobs.Job

data PollerConfig es = PollerConfig
  { queueName :: Text
  -- ^ The queue to monitor
  , interval :: Int
  -- ^ In µ-seconds.
  , poolSettings :: PoolboySettings (Eff es)
  }

mkPollerConfig :: Log :> es => Text -> PollerConfig es
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

monitorQueue :: (IOE :> es, Log :> es, WithConnection :> es) => PollerConfig es -> (Job -> Eff es ()) -> Eff es ()
monitorQueue config handler = withPoolboy config.poolSettings waitingStopFinishWorkers $ \workQueue ->
  loop workQueue
  where
    loop workQueue = do
      mJob <- readJob config.queueName
      case mJob of
        Just job -> do
          Log.logInfo "Dispatching job" $
            object
              [ "job_id" .= job.id
              , "job_message" .= job.message
              ]
          enqueue workQueue (handler job)
        Nothing -> do
          liftIO $ threadDelay config.interval
          loop workQueue
