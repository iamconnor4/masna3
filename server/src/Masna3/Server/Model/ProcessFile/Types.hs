{-# OPTIONS_GHC -Wno-orphans #-}

module Masna3.Server.Model.ProcessFile.Types
  ( ProcessFile (..)
  , newProcessFile
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
import Masna3.Api.File.FileId
import Masna3.Api.Process.ProcessId
import Masna3.Api.ProcessFile.ProcessFileId

data ProcessFile = ProcessFile
  { processFileId :: ProcessFileId
  , processId :: ProcessId
  , fileId :: FileId
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "process_files"] ProcessFile)

newProcessFile
  :: (IOE :> es, Time :> es)
  => ProcessId
  -> FileId
  -> Eff es ProcessFile
newProcessFile processId fileId = do
  processFileId <- newProcessFileId
  createdAt <- Time.currentTime
  let updatedAt = Nothing
  pure ProcessFile{..}
