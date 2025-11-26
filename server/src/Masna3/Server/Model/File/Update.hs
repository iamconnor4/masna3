{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Masna3.Server.Model.File.Update where

import Data.Time
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.PostgreSQL
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Masna3.Api.ArchivedFile.ArchivedFileId
import Masna3.Api.File.FileId

import Masna3.Server.Model.File.Types

insertFile :: (IOE :> es, WithConnection :> es) => File -> Eff es ()
insertFile file = void $ execute (_insert @File) file

confirmFile :: (IOE :> es, WithConnection :> es) => FileId -> UTCTime -> Eff es ()
confirmFile fileId timestamp = void $ execute q (timestamp, fileId)
  where
    q =
      [sql|
        UPDATE files SET status = 'uploaded', uploaded_at = ?
        WHERE file_id = ?;
       |]

deleteFile :: (IOE :> es, Time :> es, WithConnection :> es) => FileId -> Eff es ()
deleteFile fileId = do
  timestamp <- Time.currentTime
  archivedFileId <- newArchivedFileId
  void $ execute q (fileId, archivedFileId, timestamp)
  where
    q =
      [sql|
        WITH deleted AS (
          DELETE FROM files
          WHERE file_id = ?
          RETURNING *
        )
        INSERT INTO archived_files
        (archived_file_id, created_at, reason, payload)
        SELECT ?, ?, 'deleted', to_jsonb(deleted.*)
        FROM deleted;
      |]
