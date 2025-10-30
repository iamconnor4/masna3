module Masna3.Jobs.Job where

import Data.Aeson
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Newtypes
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as DB
import GHC.Generics

-- |  A PGMQ job
data Job = Job
  { id :: Integer
  , enqueuedAt :: UTCTime
  , visibilityTimeout :: UTCTime
  , message :: Text
  , retries :: Int
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToJSON)

data Headers = Headers
  { retries :: Int
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving
    (FromField)
    via Aeson Headers

instance FromRow Job where
  fromRow = do
    id <- field
    _readCount <- field @Integer
    enqueuedAt <- field
    visibilityTimeout <- field
    message <- field
    headers <- field @Headers
    pure Job{id, visibilityTimeout, enqueuedAt, message, retries = headers.retries}

insertJob
  :: (FromJSON payload, WithConnection :> es)
  => payload
  -- ^ Job payload
  -> Text
  -- ^ Queue name
  -> Maybe UTCTime
  -- ^ Delay for the processing of the job
  -> Eff es ()
insertJob payload queueName mDelay = do
