{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Masna3.Server.Model.Process.Types
  ( Process (..)
  , newProcess
  ) where

import Data.Time (UTCTime)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple.FromRow hiding (field)
import Database.PostgreSQL.Simple.ToRow
import Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Generics
import Masna3.Api.Owner.OwnerId
import Masna3.Api.Process.ProcessId

data Process = Process
  { processId :: ProcessId
  , ownerId :: OwnerId
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "processes"] Process)

newProcess
  :: (IOE :> es, Time :> es)
  => OwnerId
  -> Eff es Process
newProcess ownerId = do
  processId <- newProcessId
  createdAt <- Time.currentTime
  let updatedAt = Nothing
  pure Process{..}
