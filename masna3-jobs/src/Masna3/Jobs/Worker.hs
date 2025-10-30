module Masna3.Jobs.Worker
  ( WorkerConfig (..)
  , runWorker
  ) where

import Control.Exception
import Data.Aeson
import Effectful
import Effectful.Exception
import Effectful.Log
import Log.Class qualified as Log

import Masna3.Jobs.Job

data WorkerConfig m payload = WorkerConfig
  { queueName :: Text
  -- ^  Name of the pgmq queue where jobs of a certain type are waiting
  , onException :: Job -> SomeException -> m ()
  -- ^ What to do in case of failure to process the job payload
  , process :: payload -> m ()
  -- ^ How to process the job payload
  }

runWorker :: (FromJSON payload, Log :> es) => WorkerConfig (Eff es) payload -> Job -> Eff es ()
runWorker config job = Log.localData ["job_id" .= job.id, "queue" .= config.queueName] $ do
  case eitherDecodeStrictText job.message of
    Right payload -> do
      withException (config.process payload) (config.onException job)
    Left errorString -> do
      Log.logAttention "Could not decode job payload" $
        object ["error" .= errorString]
