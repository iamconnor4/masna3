{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Masna3.Server.Model.File.Query where

import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Internal.QQ
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types
import Effectful
import Effectful.PostgreSQL
import GHC.Stack
import Masna3.Api.File.FileId

import Masna3.Server.Model.File.Types

getFileById :: (IOE :> es, WithConnection :> es) => FileId -> Eff es (Maybe File)
getFileById fileId = queryOne (_selectWhere @File [[field| file_id |]]) (Only fileId)

queryOne
  :: (FromRow result, HasCallStack, IOE :> es, ToRow params, WithConnection :> es)
  => Query
  -> params
  -> Eff es (Maybe result)
queryOne q params = listToMaybe <$> query q params
