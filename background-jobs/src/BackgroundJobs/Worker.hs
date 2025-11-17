module BackgroundJobs.Worker
  ( WorkerConfig (..)
  , runWorker
  ) where

import Control.Exception
import Data.Aeson
import Effectful
import Effectful.Concurrent
import Effectful.Exception
import Effectful.Log
import Log.Class qualified as Log

import BackgroundJobs.Job

data WorkerConfig m payload = WorkerConfig
  { queueName :: Text
  -- ^  Name of the pgmq queue where jobs of a certain type are waiting
  , onException :: Job -> SomeException -> m ()
  -- ^ What to do in case of failure to process the job payload
  , process :: payload -> m ()
  -- ^ How to process the job payload
  }

runWorker
  :: (Concurrent :> es, FromJSON payload, Log :> es)
  => WorkerConfig (Eff es) payload
  -> Job
  -> Eff es ()
runWorker config job = do
  threadId <- myThreadId
  Log.logInfo "Running job worker" $
    object
      [ "job_id" .= job.id
      , "job_message" .= job.message
      , "queue_name" .= config.queueName
      , "thread_id" .= show threadId
      ]
  case fromJSON job.message of
    Success payload -> do
      withException (config.process payload) (config.onException job)
    Error errorString -> do
      Log.logAttention "Could not decode job payload" $
        object ["error" .= errorString]
