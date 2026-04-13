{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Masna3.Server.Model.Process.Query
  ( getProcessById
  ) where

import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Internal.QQ
import Database.PostgreSQL.Simple.Types
import Effectful
import Effectful.PostgreSQL
import Masna3.Api.Process.ProcessId

import Masna3.Server.Model.File.Query (queryOne)
import Masna3.Server.Model.Process.Types

getProcessById :: (IOE :> es, WithConnection :> es) => ProcessId -> Eff es (Maybe Process)
getProcessById processId = queryOne (_selectWhere @Process [[field| process_id |]]) (Only processId)
