{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Masna3.Server.Model.ProcessFile.Query
  ( getProcessFileById
  ) where

import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Internal.QQ
import Database.PostgreSQL.Simple.Types
import Effectful
import Effectful.PostgreSQL
import Masna3.Api.ProcessFile.ProcessFileId

import Masna3.Server.Model.File.Query (queryOne)
import Masna3.Server.Model.ProcessFile.Types

getProcessFileById :: (IOE :> es, WithConnection :> es) => ProcessFileId -> Eff es (Maybe ProcessFile)
getProcessFileById processFileId = queryOne (_selectWhere @ProcessFile [[field| process_file_id |]]) (Only processFileId)
