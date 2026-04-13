{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Masna3.Server.Model.Process.Update where

import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types
import Effectful
import Effectful.PostgreSQL
import Masna3.Api.Process.ProcessId (ProcessId)

import Masna3.Server.Model.Process.Types

insertProcess :: (IOE :> es, WithConnection :> es) => Process -> Eff es ()
insertProcess process = void $ execute (_insert @Process) process

deleteProcess :: (IOE :> es, WithConnection :> es) => ProcessId -> Eff es ()
deleteProcess processId = void $ execute (_delete @Process) (Only processId)

deleteProcessAndFiles :: (IOE :> es, WithConnection :> es) => ProcessId -> Eff es ()
deleteProcessAndFiles processId = do
  void $ execute q (Only processId)
  void $ execute (_delete @Process) (Only processId)
  where
    q =
      [sql|
        DELETE FROM files
        WHERE file_id IN (
          SELECT file_id from process_files WHERE process_id = ?
        )
      |]
