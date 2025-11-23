module BackgroundJobs.Poller where

import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Poolboy
import Data.Text qualified as Text
import Effectful
import Effectful.Concurrent hiding (threadDelay)
import Effectful.Log
import Effectful.PostgreSQL

import BackgroundJobs.Job
import BackgroundJobs.Worker

data PollerConfig m = PollerConfig
  { queueName :: Text
  -- ^ The queue to monitor
  , interval :: Int
  -- ^ In µ-seconds.
  , poolSettings :: PoolboySettings m
  }

mkPollerConfig :: Text -> PollerConfig IO
mkPollerConfig queueName =
  let poolSettings =
        PoolboySettings
          { workersCount = CapabilitiesWCS
          , workQueueName = Text.unpack queueName
          , logger = \x -> putStrLn $ "************* " <> show x
          }
   in PollerConfig
        { queueName
        , interval = 30000 -- 30ms
        , poolSettings
        }

monitorQueue
  :: (Concurrent :> es, FromJSON job, IOE :> es, Log :> es, WithConnection :> es)
  => PollerConfig IO
  -> WorkerConfig (Eff es) job
  -> Eff es ()
monitorQueue pollerConfig workerConfig =
  withEffToIO (ConcUnlift Ephemeral Unlimited) $ \unlift ->
    liftIO $
      monitorQueueIO
        pollerConfig
        (unlift $ readJob pollerConfig.queueName)
        (unlift . runWorker workerConfig)

monitorQueueIO
  :: PollerConfig IO
  -> IO (Maybe job)
  -> (job -> IO ())
  -> IO ()
monitorQueueIO pollerConfig readJob' runWorker' =
  withPoolboy pollerConfig.poolSettings waitingStopFinishWorkers $ \workQueue ->
    loop workQueue
  where
    loop workQueue = do
      mJob <- readJob'
      case mJob of
        Just job -> do
          enqueue workQueue (runWorker' job)
          loop workQueue
        Nothing -> do
          threadDelay pollerConfig.interval
          loop workQueue
