{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module BackgroundJobs.Queue where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types
import Effectful
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as DB
import GHC.Generics

data Queue = Queue
  { name :: Text
  , isPartitioned :: Bool
  , isUnlogged :: Bool
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass
    (FromRow)

createQueue
  :: (IOE :> es, WithConnection :> es)
  => Text
  -> Eff es ()
createQueue queueName = do
  _ :: [Only ()] <- DB.query q (Only queueName)
  pure ()
  where
    q =
      [sql|
    SELECT * FROM pgmq.create(?)
    |]

listQueues
  :: (IOE :> es, WithConnection :> es)
  => Eff es (Set Queue)
listQueues = do
  result <- DB.query_ q
  pure $ Set.fromList result
  where
    q =
      [sql|
        SELECT * FROM pgmq.list_queues()
        |]
