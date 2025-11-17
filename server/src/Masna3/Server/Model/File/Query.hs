{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Masna3.Server.Model.File.Query
  ( getFileById
  , listExpiredFiles
  ) where

import Data.List
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Internal.QQ
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types
import Effectful
import Effectful.PostgreSQL
import Effectful.Time
import GHC.Stack
import Masna3.Api.File.FileId

import Masna3.Server.Model.File.Types

getFileById :: (IOE :> es, WithConnection :> es) => FileId -> Eff es (Maybe File)
getFileById fileId = queryOne (_selectWhere @File [[field| file_id |]]) (Only fileId)

listExpiredFiles :: (IOE :> es, Time :> es, WithConnection :> es) => Eff es (List File)
listExpiredFiles = do
  now <- currentTime
  query q (Only now)
  where
    q =
      [sql|
          SELECT file_id
               , owner_id
               , filename
               , path
               , bucket
               , mimetype
               , created_at
               , updated_at
               , uploaded_at
          WHERE created_at >= ?
            AND uploaded_at IS NULL
        |]

queryOne
  :: (FromRow result, HasCallStack, IOE :> es, ToRow params, WithConnection :> es)
  => Query
  -> params
  -> Eff es (Maybe result)
queryOne q params = listToMaybe <$> query q params
