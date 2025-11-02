{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Masna3.Jobs.Queue where

import Data.Aeson
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
