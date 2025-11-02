{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Masna3.Jobs.Job where

import Data.Aeson
import Data.List qualified as List
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.Types
import Effectful
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as DB
import GHC.Generics

-- |  A PGMQ job
data Job = Job
  { id :: Integer
  , readCount :: Integer
  , enqueuedAt :: UTCTime
  , visibilityTimeout :: UTCTime
  , message :: Value
  , headers :: Maybe Value
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromRow)

insertJob
  :: (IOE :> es, WithConnection :> es, ToField payload)
  => Text
  -- ^ Queue name
  -> payload
  -- ^ Job payload
  -> Eff es ()
insertJob queueName payload = do
  _ :: [Only Int] <- DB.query q (queueName, payload)
  pure ()
  where
    q =
      [sql|
        SELECT * FROM pgmq.send(?, ?)
    |]

archiveJob
  :: (IOE :> es, WithConnection :> es)
  => Text
  -> Integer
  -> Eff es Bool
archiveJob queueName msgId = do
  result <- DB.query q (queueName, msgId)
  case List.uncons result of
    Just (Only b, _) -> pure b
    Nothing -> pure False
  where
    q =
      [sql|
        SELECT * FROM pgmq.archive(?, ?));
    |]

readJob
  :: (IOE :> es, WithConnection :> es)
  => Text
  -- ^ Queue name
  -> Eff es (Maybe Job)
readJob queueName = do
  -- Visibility Timeout is set to twelve hours
  listToMaybe <$> DB.query q (queueName, 43200 :: Int, 1 :: Int)
  where
    q =
      [sql|
        SELECT * FROM pgmq.read(?::text, ?, ?)
    |]
